;;;; Utility functions for biometrics
(cl:in-package #:jfh-web-auth)

(defun generate-challenge (&optional (length 32))
  "Return a (simple-array (unsigned-byte 8) (*)) of random bytes."
  (ironclad:random-data length))

(defun base64url-encode (octets)
  "Encode OCTETS (a (simple-array (unsigned-byte 8) (*))) to base64url string."
  (let* ((b64 (cl-base64:usb8-array-to-base64-string octets))
         (no-pad (string-right-trim "=" b64)))
    (substitute #\- #\+ (substitute #\_ #\/ no-pad))))

(defun base64url-decode (string)
  "Decode base64url STRING to (simple-array (unsigned-byte 8) (*))."
  (let* ((s (substitute #\+ #\- (substitute #\/ #\_ string)))
         (pad-len (mod (- 4 (mod (length s) 4)) 4))
         (padded (concatenate 'string s (make-string pad-len :initial-element #\=))))
    (cl-base64:base64-string-to-usb8-array padded)))

(defun respond-json (data)
  (write-string (cl-json:encode-json-alist-to-string data)))

(defun find-user-by-id (id)
  "Return a user object for the given ID. For now, always return canned data."
  (jfh-user:get-user-info (make-instance 'jfh-user:application-user-id :user-id id)))

(defun user-id->bytes (user-id)
  "Return the stable byte-array user ID for WebAuthn operations.
This must be a (simple-array (unsigned-byte 8) (*)) of length 1–64 bytes."
  (cond
    ;; Already correct type
    ((typep user-id '(simple-array (unsigned-byte 8) (*)))
     user-id)
    ;; Convert from a list of integers
    ((and (listp user-id)
          (every (lambda (x) (and (integerp x) (<= 0 x 255))) user-id)) ;; TODO - more direct way to check if an integer list?
     (coerce user-id '(simple-array (unsigned-byte 8) (*))))
    ;; Convert from a string, convert to UTF‑8 bytes
    ((stringp user-id)
     (babel:string-to-octets user-id :encoding :utf-8))
    (t
     (error "Invalid user ID format: ~S" user-id))))

(defun current-rp-id ()
  "Return RP ID, e.g. \"webauthn.test\""
  "webdevaxis.tech") ;; TODO - make this a DEFPARAMETER

(defun webauthn-register-start (user-id)
  "Register user biometrics for webauthn.
Return the generated challenge and the JSON response."
  (let ((challenge (generate-challenge))
        (rp-id (current-rp-id))
        (rp-name "Chasi") ;; TODO defparam this
        (user-id-bytes (user-id->bytes user-id))
        (user-name "jhiltington")
        (user-display-name "John F Hiltington, III")
        )

    ;; NOTE - persist the challenge across requests; I don't want to use session for this.
    (jfh-store:save-object (make-instance 'jfh-web-auth:webauthn-challenge :user-id user-id :data-id (format nil "~D" (random 100)) :challenge challenge))

    (respond-json
     `(("publicKey"
        . (("challenge" . ,(base64url-encode challenge))
           ("rp" . (("name" . ,rp-name)
                    ("id" . ,rp-id)))
           ("user" . (("id" . ,(base64url-encode user-id-bytes))
                      ("name" . ,user-name)
                      ("displayName" . ,user-display-name)
                      ))
           ("pubKeyCredParams"
            . ((("type" . "public-key") ("alg" . -7))
               (("type" . "public-key") ("alg" . -257))))
           ("timeout" . 60000)
           ("attestation" . "none")
           ("authenticatorSelection"
            . (("userVerification" . "preferred")))))))))

(defun parse-json-body (request)
  "Return the JSON request body as an alist."
  (let* ((raw (tbnl:raw-post-data :request request))
         (text (babel:octets-to-string raw :encoding :utf-8)))
    (cl-json:decode-json-from-string text)))

(defun store-webauthn-credential (&key user-id credential-id public-key sign-count)
  "Save biometrics credentials.
  CREDENTIAL-ID is raw bytes, PUBLIC-KEY is COSE key map"
  (jfh-user:save-new-application-user
   (make-instance 'webauthn-info-readable :user-id user-id :public-key-readable public-key :credential-id credential-id :sign-count sign-count)))

(defun expected-origin ()
  "Must match what the browser sees"
  (let ((ssl-port (jfh-web-server:ssl-port (jfh-web-server:web-configuration jfh-web-server:*web-application*))))
    (format nil "https://chasi.webdevaxis.tech:~D" ssl-port))) ;; TODO we need to get this from configuration; also handle if port is 0 or 443

(defun webauthn-register-finish (request user-id)
  (let* ((expected-challenge (challenge (jfh-store:make-instance* 'jfh-web-auth:webauthn-challenge :user-id user-id)))
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
        (break)
        (unless (equal client-challenge (base64url-encode (coerce expected-challenge '(simple-array (unsigned-byte 8) (*)))))
          (error "Challenge mismatch: ~A." client-challenge))

        (unless (string= client-origin (expected-origin))
          (error "Origin mismatch: ~A." client-origin))

        (multiple-value-bind (fmt auth-data att-stmt)
            (parse-attestation-object attestation-object)
          (declare (ignore fmt att-stmt)) ;; for "none" attestation

          (multiple-value-bind (rp-id-hash flags sign-count aaguid cred-id cred-pubkey)
              (parse-authenticator-data auth-data)
            (declare (ignore flags aaguid cred-id))

            (let* ((rp-id (current-rp-id))
                   (rp-id-hash-expected (sha256-bytes (string-to-utf8 rp-id))))
              (unless (equalp rp-id-hash rp-id-hash-expected)
                (error "RP ID hash mismatch: ~A." rp-id-hash)))

            ;; NOTE: can optionally verify attestation statement for non-"none" formats

            (store-webauthn-credential :user-id user-id :credential-id raw-id :public-key cred-pubkey :sign-count sign-count)

            (respond-json '(("status" . "ok")))))))))

(defun webauthn-login-start (user-id)
  (let ((challenge (generate-challenge))
        (rp-id (current-rp-id))
        (credentials (user-webauthn-credentials user-id)))
    
    ;; NOTE - persist the challenge across requests; I don't want to use session for this.
    (jfh-store:save-object (make-instance 'jfh-web-auth:webauthn-challenge :user-id user-id :data-id (format nil "~D" (random 100)) :challenge challenge))

    ;; (setf (gethash 'webauthn-login-challenge *session-user-map*) challenge)
    ;; (setf (gethash 'webauthn-login-user-id *session-user-map*) (the-user-id the-user))

    (respond-json
     `(("publicKey"
        . (("challenge" . ,(base64url-encode challenge))
           ("rpId" . ,rp-id)
           ("timeout" . 60000)
           ("userVerification" . "preferred")
           ("allowCredentials"
            . ,(mapcar
                (lambda (cred)
                  `(("type" . "public-key")
                    ("id" . ,(base64url-encode
                              (coerce (credential-id-bytes cred) '(simple-array (unsigned-byte 8) (*)))))))
                credentials))))))))

;; ;;; login finish
;; (defun credential-public-key (cred)
;;   (declare (ignore cred))
;;   (error "credential-public-key not implemented"))

;; (defun credential-sign-count (cred)
;;   (declare (ignore cred))
;;   (error "credential-sign-count not implemented"))

;; (defun credential-user (cred)
;;   (declare (ignore cred))
;;   (error "credential-user not implemented"))

(defun log-in-user (user-id)
  ;; (error "log-in-user not implemented")
  (format t "Pretend the user is logged in now. ~%Here's the user ID: ~A~%" user-id))

;; (defun cose-key->ironclad-ec-key (cose-key) ;; UNUSED
;;   "Convert a COSE EC2 key (ES256) to an Ironclad ECDSA key.

;; COSE fields (RFC 8152):
;;   1  = kty   (2 = EC2)
;;   3  = alg   (-7 = ES256)
;;  -1  = crv   (1 = P-256)
;;  -2  = x     (bstr)
;;  -3  = y     (bstr)"
;;   (let* ((kty (gethash 1 cose-key))
;;          (alg (gethash 3 cose-key))
;;          (crv (gethash -1 cose-key))
;;          (x   (gethash -2 cose-key))
;;          (y   (gethash -3 cose-key)))
;;     (unless (and (= kty 2) (= alg -7) (= crv 1))
;;       (error "Unsupported COSE key: kty=~A alg=~A crv=~A" kty alg crv))
;;     ;; Ironclad expects uncompressed point: 0x04 || X || Y
;;     (let* ((x-bytes (coerce x '(simple-array (unsigned-byte 8) (*))))
;;            (y-bytes (coerce y '(simple-array (unsigned-byte 8) (*))))
;;            (point   (concatenate '(simple-array (unsigned-byte 8) (*))
;;                                  #(4) x-bytes y-bytes)))
;;       (ironclad:make-public-key
;;        :secp256r1
;;        :q point :y y-bytes)
;;       ;; (ironclad:make-public-key
;;       ;;  :ecdsa
;;       ;;  :curve :secp256r1
;;       ;;  :q point)
;;       )))

(defun cose-key->xy (cose-key)
  "Extract X and Y from COSE EC2 key as byte vectors."
  (let ((x (gethash -2 cose-key))
        (y (gethash -3 cose-key)))
    (values (coerce x '(simple-array (unsigned-byte 8) (*)))
            (coerce y '(simple-array (unsigned-byte 8) (*))))))

(defun verify-signature (cose-public-key signed-bytes signature)
  "Verify ES256 signature using OpenSSL via C helper.

COSE-PUBLIC-KEY is the COSE EC2 key map.
SIGNED-BYTES is authenticatorData || SHA256(clientDataJSON).
SIGNATURE is the DER-encoded ECDSA signature from WebAuthn."
  (multiple-value-bind (x-bytes y-bytes)
      (cose-key->xy cose-public-key)
    ;; NEXT - work on integrating C part
    (verify-es256 x-bytes y-bytes signed-bytes signature)))

;; (defun verify-signature-OLD (cose-public-key signed-bytes signature)
;;   "Verify an ES256 (ECDSA P-256 SHA-256) signature.

;; COSE-PUBLIC-KEY is the COSE key map (from cl-cbor).
;; SIGNED-BYTES is the exact byte vector (authData || SHA256(clientDataJSON)).
;; SIGNATURE is the raw ECDSA signature from WebAuthn (r||s)."
;;   (let* ((pubkey (cose-key->ironclad-ec-key cose-public-key))
;;          ;; WebAuthn ES256 signatures are ASN.1 DER *or* raw r||s depending on platform.
;;          ;; Most browsers use DER. Ironclad expects DER.
;;          ;; If you ever see raw r||s, you’ll need to wrap it into DER first.
;;          (digest (ironclad:digest-sequence :sha256 signed-bytes)))
;;     (handler-case
;;         (ironclad:verify-signature pubkey digest signature)
;;       (error () nil))))

;; ;; (defun verify-signature (public-key signed-bytes signature)
;; ;;   ;; Wire this to your crypto lib; public-key is COSE key map
;; ;;   (declare (ignore public-key signed-bytes signature))
;; ;;   (error "verify-signature not implemented"))

(defun validate-client-data-json (client-data-json user-id)
  (multiple-value-bind (client-type client-challenge client-origin)
      (parse-client-data-json client-data-json)
    (unless (string= client-type "webauthn.get")
      (error "Invalid clientData type: ~A" client-type))
    (let* ((expected-challenge (challenge (jfh-store:make-instance* 'jfh-web-auth:webauthn-challenge :user-id user-id)))
           (coerced-expected-challenge (coerce expected-challenge '(simple-array (unsigned-byte 8) (*)))))
      (unless (equal client-challenge (base64url-encode coerced-expected-challenge))
        (error "Challenge mismatch: ~A" client-challenge)))
    (unless (string= client-origin (expected-origin))
      (error "Origin mismatch: ~A." client-origin))))

(defun validate-rpid (rp-id-hash)
  (let* ((rp-id (current-rp-id))
         (rp-id-hash-expected (sha256-bytes (string-to-utf8 rp-id))))
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
          
          (multiple-value-bind (rp-id-hash flags sign-count aaguid cred-id cred-pubkey)
              (parse-authenticator-data auth-data)
            (declare (ignore flags aaguid cred-id cred-pubkey))
            (validate-rpid rp-id-hash)
            
            (let* ((cred (find-credential-by-id raw-id)) ;; we also should have the user ID by this point too
                   (stored-pubkey (public-key cred))
                   (stored-sign-count (sign-count cred)))
              (let* ((client-hash (sha256-bytes client-data-json))
                     (signed-bytes (concatenate '(simple-array (unsigned-byte 8) (*))
                                                auth-data
                                                client-hash)))
                (unless (verify-signature stored-pubkey signed-bytes signature)
                  (error "Invalid signature")))
              (when (and stored-sign-count
                         (> sign-count stored-sign-count))
                (update-credential-sign-count cred sign-count))

              (log-in-user (jfh-store:user-id cred))

              (format t "Finished login!~%")
              (respond-json '(("status" . "ok"))))))))))

;;   #| minimal js to trigger biometrics
;;   navigator.credentials.create(options.publicKey)
;;   |#

;;   #| minimal JS code
;;   <script>
;; async function startRegistration() {
;;   // 1. Fetch the JSON from your server
;;   const resp = await fetch("/webauthn/register/start");
;;   const options = await resp.json();

;;   // 2. Convert base64url strings → ArrayBuffers
;;   options.publicKey.challenge = base64urlToBuffer(options.publicKey.challenge);
;;   options.publicKey.user.id = base64urlToBuffer(options.publicKey.user.id);

;;   for (const cred of options.publicKey.pubKeyCredParams) {
;;     // nothing to convert here
;;   }

;;   // 3. Call WebAuthn
;;   const credential = await navigator.credentials.create(options);

;;   console.log("Created credential:", credential);
;; }

;; // Helper: base64url → ArrayBuffer
;; function base64urlToBuffer(b64url) {
;;   const padding = "=".repeat((4 - (b64url.length % 4)) % 4);
;;   const b64 = (b64url + padding).replace(/-/g, "+").replace(/_/g, "/");
;;   const raw = atob(b64);
;;   const buffer = new ArrayBuffer(raw.length);
;;   const bytes = new Uint8Array(buffer);
;;   for (let i = 0; i < raw.length; i++) bytes[i] = raw.charCodeAt(i);
;;   return buffer;
;; }
;; </script>
;;   |#

;;   #| minimal hard-coded testing
;;   mind.publicKey.challenge = base64urlToBuffer(mine.publicKey.challenge);
;;   mind.publicKey.user.id = base64urlToBuffer(mine.publicKey.user.id);

;;   const credential = await navigator.credentials.create(mind);

;;   console.log("Created credential:", credential);
;; |#

;; ;; some helpers
;; ;; (defun store-webauthn-credential (user credential-id public-key sign-count aaguid fmt attStmt)
;; ;;   ;; Insert into DB and return object
;; ;;   )

(defun user-webauthn-credentials (user-id)
  "Return list/vector of credentials for user"
  (let* ((readable-object (jfh-store:make-instance* 'webauthn-info-readable :user-id user-id))
         (object (make-instance 'webauthn-info
                                :credential-id (credential-id readable-object)
                                :user-id (jfh-store:user-id readable-object)
                                :public-key (public-key-readable readable-object)
                                :sign-count (sign-count readable-object))))
    (list object)))

(defun find-credential-by-id (credential-id)
  "Lookup credential for user by credential-id bytes"
  ;; (let* ((readable-object (jfh-store:make-instance* 'proto-webauthn-web-app::webauthn-info-readable :user-id (the-user-id (current-user))))
  ;;        (object (make-instance 'proto-webauthn-web-app::webauthn-info :credential-id (credential-id readable-object) :user-id (jfh-store:user-id readable-object) :public-key (public-key-readable readable-object) :sign-count (sign-count readable-object))))
  ;;   object)
  (let* ((readable-object (find-user-by-credential credential-id))
         (object (make-instance 'webauthn-info :credential-id (credential-id readable-object) :user-id (jfh-store:user-id readable-object) :public-key (public-key-readable readable-object) :sign-count (sign-count readable-object))))
    object))

(defun update-credential-sign-count (credential new-count)
  "Persist updated sign count"
  (let* ((readable-object (jfh-store:make-instance* 'webauthn-info-readable :user-id (jfh-store:user-id credential))))
    (setf (sign-count readable-object) new-count)
    (jfh-store:save-object readable-object)))

(defun extract-credential-id (request)
  "Return the rawId (base64url string) from the WebAuthn assertion request."
  (gethash "rawId" request))

(defun find-user-by-credential (credential-id-bytes)
  "Return the user object associated with this credential row."
  (jfh-user:get-user-info (make-instance 'application-user-webauthn-credentials
		                         :user-credential-id credential-id-bytes)))

(defun find-user-from-request (request)
  (let* ((raw-id (extract-credential-id request))
         (cred-id-bytes (base64url-decode raw-id)))
    (find-user-by-credential cred-id-bytes)))

;; (defun current-session()
;;   )

(defun credential-id-bytes (cred)
  (credential-id cred))

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
  (let* ((rp-id-hash (subseq bytes 0 32))
         (flags (aref bytes 32))
         (sign-count (bytes-to-uint32 (subseq bytes 33 37)))
         (at-flag (logbitp 6 flags)))
    (if at-flag
        (let* ((offset 37) ;; TODO put the offsets into an array
               (aaguid (subseq bytes offset (+ offset 16)))
               (offset (+ offset 16))
               (cred-id-len (bytes-to-uint16 (subseq bytes offset (+ offset 2))))
               (offset (+ offset 2))
               (cred-id (subseq bytes offset (+ offset cred-id-len)))
               (offset (+ offset cred-id-len))
               (cred-pubkey-bytes (subseq bytes offset))
               (cred-pubkey (cbor:decode cred-pubkey-bytes)))
          (values rp-id-hash flags sign-count aaguid cred-id cred-pubkey))
        (values rp-id-hash flags sign-count nil nil nil))))

;; ;;; actual page handlers

;; ;; (tbnl:define-easy-handler (webauthn-register-start-handler :uri "/webauthn/register/start") ()
;; ;;   (webauthn-register-start))

;; (tbnl:define-easy-handler (webauthn-register-finish-handler :uri "/webauthn/register/finish") ()
;;   (webauthn-register-finish tbnl:*request*))

;; (tbnl:define-easy-handler (webauthn-login-start-handler :uri "/webauthn/login/start") ()
;;   (webauthn-login-start tbnl:*request*))

;; (tbnl:define-easy-handler (webauthn-login-finish-handler :uri "/webauthn/login/finish") ()
;;   (webauthn-login-finish tbnl:*request*))
