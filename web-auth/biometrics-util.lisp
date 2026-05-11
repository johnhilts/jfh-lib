;;;; Utility functions for biometrics
(cl:in-package #:jfh-web-auth)

(defun generate-challenge (&optional (length 32))
  "Return a (simple-array (unsigned-byte 8) (*)) of random bytes."
  (ironclad:random-data length))

(defun base64url-encode (octets)
  "Encode OCTETS (a (simple-array (unsigned-byte 8) (*))) to base64url string.
Trim trailing = padding.
s/+/-/g and s-/-_-g"
  (let* ((b64 (cl-base64:usb8-array-to-base64-string octets))
         (no-pad (string-right-trim "=" b64)))
    (substitute #\- #\+ (substitute #\_ #\/ no-pad))))

(defun base64url-decode (string)
  "Decode base64url STRING to (simple-array (unsigned-byte 8) (*)).
Add = padding.
s/-/+/g and s-_-/-g"
  (let* ((s (substitute #\+ #\- (substitute #\/ #\_ string)))
         (pad-len (mod (- 4 (mod (length s) 4)) 4))
         (padded (concatenate 'string s (make-string pad-len :initial-element #\=))))
    (cl-base64:base64-string-to-usb8-array padded)))

(defun respond-json (data)
  (write-string (cl-json:encode-json-alist-to-string data)))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets ()
  '(simple-array octet (*)))

(defun user-id->bytes (user-id)
  "Return the stable byte-array user ID for WebAuthn operations.
This must be a (simple-array (unsigned-byte 8) (*)) of length 1–64 bytes."
  (flet ((is-correct-type ()
           (typep user-id 'octets))
         (is-integer-list ()
           (and (listp user-id)
                (every (lambda (e) `(typep ,e octet)) user-id))))
    (cond
      ((is-correct-type)
       user-id)
      ((is-integer-list)
       (coerce user-id 'octets))
      ((stringp user-id)
       (string-to-utf8 user-id))
      (t
       (error "Invalid User ID format: ~S" user-id)))))

;; TODO - get these from an sexp too
(defparameter *webauthn-user-name* "jhiltington")
(defparameter *webauthn-user-display-name* "John F Hiltington, III")

(defun webauthn-register-start (user-id)
  "Register user biometrics for webauthn.
Return the generated challenge and the JSON response."
  (let ((challenge (generate-challenge))
        (user-id-bytes (user-id->bytes user-id)))

    ;; NOTE - persist the challenge across requests; I don't want to use session for this.
    (jfh-store:save-object (make-instance 'webauthn-challenge :user-id user-id :challenge challenge))

    (respond-json
     `(("publicKey"
        . (("challenge" . ,(base64url-encode challenge))
           ("rp" . (("name" . ,(jfh-web-server:site-display-name jfh-auth:*webauthn-configuration*))
                    ("id" . ,(jfh-web-server:site-registrable-domain jfh-auth:*webauthn-configuration*))))
           ("user" . (("id" . ,(base64url-encode user-id-bytes))
                      ("name" . ,*webauthn-user-name*)
                      ("displayName" . ,*webauthn-user-display-name*)))
           ("pubKeyCredParams"
            . ((("type" . "public-key") ("alg" . -7))
               (("type" . "public-key") ("alg" . -257))))
           ("timeout" . ,(jfh-auth:timeout jfh-auth:*webauthn-configuration*))
           ("attestation" . "none")
           ("authenticatorSelection"
            . (("userVerification" . "preferred")))))))))

(defun parse-json-body (request)
  "Return the JSON request body as an alist."
  (let* ((raw (tbnl:raw-post-data :request request))
         (text (utf8-bytes-to-string raw)))
    (cl-json:decode-json-from-string text)))

(defun store-webauthn-credential (&key user-id credential-id public-key sign-count)
  "Save biometrics credentials.
  CREDENTIAL-ID is raw bytes, PUBLIC-KEY is COSE key map"
  (jfh-user:save-new-application-user
   (make-instance 'jfh-auth:webauthn-info-readable :user-id user-id :public-key-readable public-key :credential-id credential-id :sign-count sign-count)))

(defun expected-origin ()
  "Must match what the browser sees"
  (let ((ssl-port (jfh-web-server:ssl-port (jfh-web-server:web-configuration jfh-web-server:*web-application*))))
    (if (member ssl-port '(0 80 443))
        (jfh-web-server:site-origin jfh-auth:*webauthn-configuration*)
        (format nil "~A:~D" (jfh-web-server:site-origin jfh-auth:*webauthn-configuration*) ssl-port))))

(defun webauthn-register-finish (request user-id)
  (let* ((expected-challenge (jfh-auth:challenge (jfh-store:make-instance* 'jfh-auth:webauthn-challenge :user-id user-id)))
         (body (parse-json-body request))
         (raw-id-b64 (cdr (assoc :raw-id body)))
         (response (cdr (assoc :response body)))
         (attestation-b64 (cdr (assoc :attestation-object response)))
         (client-data-b64 (cdr (assoc :client-data-+json+ response))))

    (let ((raw-id (base64url-decode raw-id-b64))
          (attestation-object (base64url-decode attestation-b64))
          (client-data-json (base64url-decode client-data-b64)))

      (multiple-value-bind (client-type client-challenge client-origin)
          (parse-client-data-json client-data-json)
        (unless (string= client-type "webauthn.create")
          (error "Invalid clientData type: ~A." client-type))
        (unless (equal client-challenge (base64url-encode (coerce expected-challenge 'octets)))
          (error "Challenge mismatch: ~A." client-challenge))

        (unless (string= client-origin (expected-origin))
          (error "Origin mismatch: ~A." client-origin))

        (multiple-value-bind (fmt auth-data att-stmt)
            (parse-attestation-object attestation-object)
          (declare (ignore fmt att-stmt)) ;; for "none" attestation

          (multiple-value-bind (rp-id-hash flags sign-count aaguid cred-id cred-pubkey)
              (parse-authenticator-data auth-data)
            (declare (ignore flags aaguid cred-id))

            (let ((rp-id-hash-expected (sha256-bytes (string-to-utf8 (jfh-web-server:site-registrable-domain jfh-auth:*webauthn-configuration*)))))
              (unless (equalp rp-id-hash rp-id-hash-expected)
                (error "RP ID hash mismatch: ~A." rp-id-hash)))

            ;; NOTE: can optionally verify attestation statement for non-"none" formats

            (store-webauthn-credential :user-id user-id :credential-id raw-id :public-key cred-pubkey :sign-count sign-count)

            (respond-json '(("status" . "ok")))))))))

(defun webauthn-login-start (user-id)
  (let ((challenge (generate-challenge))
        (credentials (user-webauthn-credentials user-id)))
    
    ;; NOTE - persist the challenge across requests; I don't want to use session for this.
    (jfh-store:save-object (make-instance 'jfh-auth:webauthn-challenge :user-id user-id :challenge challenge))

    (respond-json
     `(("publicKey"
        . (("challenge" . ,(base64url-encode challenge))
           ("rpId" . ,(jfh-web-server:site-registrable-domain jfh-auth:*webauthn-configuration*))
           ("timeout" . ,(jfh-auth:timeout jfh-auth:*webauthn-configuration*))
           ("userVerification" . "preferred")
           ("allowCredentials"
            . ,(mapcar
                (lambda (cred)
                  `(("type" . "public-key")
                    ("id" . ,(base64url-encode
                              (coerce (credential-id-bytes cred) 'octets)))))
                (list credentials)))))))))

(defun cose-key->xy (cose-key)
  "Extract X and Y from COSE EC2 key as byte vectors."
  (let ((x (gethash -2 cose-key))
        (y (gethash -3 cose-key)))
    (values (coerce x 'octets)
            (coerce y 'octets))))

(defun verify-signature (cose-public-key signed-bytes signature)
  "Verify ES256 signature using OpenSSL via C helper.

COSE-PUBLIC-KEY is the COSE EC2 key map.
SIGNED-BYTES is authenticatorData || SHA256(clientDataJSON).
SIGNATURE is the DER-encoded ECDSA signature from WebAuthn."
  (multiple-value-bind (x-bytes y-bytes)
      (cose-key->xy cose-public-key)
    (jfh-auth:verify-es256 x-bytes y-bytes signed-bytes signature)))

(defun validate-client-data-json (client-data-json user-id)
  (multiple-value-bind (client-type client-challenge client-origin)
      (parse-client-data-json client-data-json)
    (unless (string= client-type "webauthn.get")
      (error "Invalid clientData type: ~A" client-type))
    (let* ((expected-challenge (jfh-auth:challenge (jfh-store:make-instance* 'jfh-auth:webauthn-challenge :user-id user-id)))
           (coerced-expected-challenge (coerce expected-challenge 'octets)))
      (unless (equal client-challenge (base64url-encode coerced-expected-challenge))
        (error "Challenge mismatch: ~A" client-challenge)))
    (unless (string= client-origin (expected-origin))
      (error "Origin mismatch: ~A." client-origin))))

(defun validate-rpid (rp-id-hash)
  (let ((rp-id-hash-expected (sha256-bytes (string-to-utf8 (jfh-web-server:site-registrable-domain jfh-auth:*webauthn-configuration*)))))
    (unless (equalp rp-id-hash rp-id-hash-expected)
      (error "RP ID hash mismatch: ~A." rp-id-hash))))

(defun webauthn-login-finish (request user-id)
  (let ((body (parse-json-body request)))
    (let ((response (cdr (assoc :response body))))
      (let ((auth-data-b64 (cdr (assoc :authenticator-data response)))
            (client-data-b64 (cdr (assoc :client-data-+json+ response)))
            (signature-b64 (cdr (assoc :signature response))))
        (let* ((raw-id-b64 (cdr (assoc :raw-id body)))
               (raw-id (base64url-decode raw-id-b64))
               (auth-data (base64url-decode auth-data-b64))
               (client-data-json (base64url-decode client-data-b64))
               (signature (base64url-decode signature-b64)))

          (validate-client-data-json client-data-json user-id)
          
          (multiple-value-bind (rp-id-hash flags new-sign-count aaguid cred-id cred-pubkey)
              (parse-authenticator-data auth-data)
            (declare (ignore flags aaguid cred-id cred-pubkey))
            (validate-rpid rp-id-hash)
            
            (let* ((user-credential (find-credential-by-id raw-id)) ;; we also should have the user ID by this point too
                   (stored-pubkey (jfh-auth:public-key user-credential))
                   (stored-sign-count (jfh-auth:sign-count user-credential)))
              (let* ((client-hash (sha256-bytes client-data-json))
                     (signed-bytes (concatenate 'octets
                                                auth-data
                                                client-hash)))
                (unless (verify-signature stored-pubkey signed-bytes signature)
                  (error "Invalid signature")))
              (when (and stored-sign-count
                         (> new-sign-count stored-sign-count))
                (update-credential-sign-count user-credential new-sign-count))

              (jfh-auth:refresh-mfa-expiration (jfh-store:user-id user-credential) 'jfh-auth:webauthn-mfa)

              (format t "Finished login!~%")
              (respond-json '(("status" . "ok"))))))))))

(defun user-webauthn-credentials (user-id)
  "Return list/vector of credentials for user"
  (let* ((readable-object (jfh-store:make-instance* 'jfh-auth:webauthn-info-readable :user-id user-id)))
    (make-instance 'jfh-auth:webauthn-info
                   :credential-id (jfh-auth:credential-id readable-object)
                   :user-id (jfh-store:user-id readable-object)
                   :public-key (jfh-auth:public-key-readable readable-object)
                   :sign-count (jfh-auth:sign-count readable-object))))

(defun find-credential-by-id (credential-id)
  "Lookup credential for user by credential-id bytes"
  (let ((readable-object (find-user-by-credential credential-id)))
    (make-instance 'jfh-auth:webauthn-info
                   :credential-id (jfh-auth:credential-id readable-object)
                   :user-id (jfh-store:user-id readable-object)
                   :public-key (jfh-auth:public-key-readable readable-object)
                   :sign-count (jfh-auth:sign-count readable-object))))

(defun update-credential-sign-count (credential new-count)
  "Persist updated sign count"
  (let* ((readable-object (jfh-store:make-instance* 'jfh-auth:webauthn-info-readable :user-id (jfh-store:user-id credential))))
    (setf (jfh-auth:sign-count readable-object) new-count)
    (jfh-store:save-object readable-object)))

(defun extract-credential-id (request)
  "Return the rawId (base64url string) from the WebAuthn assertion request."
  (gethash "rawId" request))

(defun find-user-by-credential (credential-id-bytes)
  "Return the user object associated with this credential row."
  (jfh-user:get-user-info (make-instance 'application-user-webauthn-credentials :user-credential-id credential-id-bytes)))

(defun credential-id-bytes (cred)
  (jfh-auth:credential-id cred))

;; more helpers
(defun utf8-bytes-to-string (bytes)
  "UTF-8 decoder"
  (babel:octets-to-string bytes :encoding :utf-8))

(defun string-to-utf8 (s)
  (babel:string-to-octets s :encoding :utf-8))

(defun sha256-bytes (bytes)
  (ironclad:digest-sequence :sha256 bytes))

(defun bytes-to-uint16 (bytes)
  (logior (ash (aref bytes 0) 8)
          (aref bytes 1)))

(defun bytes-to-uint32 (bytes)
  (logior (ash (aref bytes 0) 24)
          (ash (aref bytes 1) 16)
          (ash (aref bytes 2) 8)
          (aref bytes 3)))

(defun parse-client-data-json (bytes)
  "Parse client data JSON."
  (let* ((json-string (utf8-bytes-to-string bytes))
         (obj (cl-json:decode-json-from-string json-string)))
    (values
     (cdr (assoc "type" obj :test #'string-equal))
     (cdr (assoc "challenge" obj :test #'string-equal))
     (cdr (assoc "origin" obj :test #'string-equal)))))

(defun parse-attestation-object (bytes)
  "Parse attestation object (CBOR)
  attestation object is CBOR map with keys: \"fmt\", \"authData\", \"attStmt\""
  (let* ((obj (cbor:decode bytes))
         (fmt (gethash :fmt obj))
         (auth-data (gethash :authData obj))
         (att-stmt (gethash :attStmt obj)))
    (values fmt auth-data att-stmt)))

(defun parse-authenticator-data (bytes)
  "Parse authenticator data
  Layout:
  0-31: RP ID hash
  32: flags
  33-36: sign count (big-endian)
  then, attested credential data if AT flag set"
  (let ((rp-id-hash-range '(0 32))
        (flag-position 32)
        (sign-count-range '(33 37)))
    (let* ((rp-id-hash (apply #'subseq `(,bytes ,@rp-id-hash-range)))
           (flags (aref bytes flag-position))
           (sign-count (bytes-to-uint32 (apply #'subseq `(,bytes ,@sign-count-range))))
           (at-flag (logbitp 6 flags)))
      (if at-flag
          (let* ((offset (nth 1 sign-count-range))
                 (aaguid (subseq bytes offset (incf offset 16)))
                 (cred-id-len (bytes-to-uint16 (subseq bytes offset (incf offset 2))))
                 (cred-id (subseq bytes offset (incf offset cred-id-len)))
                 (cred-pubkey-bytes (subseq bytes offset))
                 (cred-pubkey (cbor:decode cred-pubkey-bytes)))
            (values rp-id-hash flags sign-count aaguid cred-id cred-pubkey))
          (values rp-id-hash flags sign-count nil nil nil)))))

