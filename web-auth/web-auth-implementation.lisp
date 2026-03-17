;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defparameter *session-user-map* (make-hash-table))

(defmethod jfh-web-server:fetch-or-create-user-session ((user-identifier jfh-user:application-user-login))
  "Establish the user session in Hunchentoot's session apparatus + in cookies."
  (let ((session-token (jfh-utility:generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token *session-user-map*) (jfh-store:user-id (jfh-user::get-secure-user-info user-identifier)))))

(defmethod jfh-web-server:fetch-or-create-user-session ((user-identifier jfh-user:application-user-fingerprint))
  "Establish the user session in Hunchentoot's session apparatus + in cookies."
  (if (tbnl:session-value 'the-session-key)
      (tbnl:session-value 'the-session-key)
      (setf
       (tbnl:session-value 'the-session-key) ;; TODO - is this thread safe??
       ;; TODO add 401 if we can't find a match
       (let ((user-id (jfh-store:user-id (jfh-user:get-secure-user-info user-identifier))))
         (remhash user-id *mfa-checks*)
         user-id))))

(defparameter *mfa-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defun needs-mfa-check (user-id)
  (let* ((last-mfa-check (gethash user-id *mfa-checks* 'not-found))
         (mfa-check-not-found (eql 'not-found last-mfa-check))
         (last-mfa-check-expired (or
                                  mfa-check-not-found
                                  (>
                                   (- (get-universal-time) last-mfa-check)
                                   (* 60 100)))))
    (or
     mfa-check-not-found
     last-mfa-check-expired)))

(defmethod jfh-web-server:prompt-mfa ((tbnl:*request* tbnl:request) user-id)
  "Redirect to MFA prompt. The conditions are: 1. No recent MFA check."
  (when (needs-mfa-setup user-id)
    (tbnl:redirect (format nil "/mfa-setup?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  (when (needs-mfa-check user-id)
    (tbnl:redirect (format nil "/prompt-mfa?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  ;; sliding MFA expiration
  (setf (gethash user-id *mfa-checks*) (get-universal-time)))

(defmethod jfh-security:encrypt ((totp-info totp-info) &optional key)
  (let ((encryption-key (or key (jfh-security:fetch-key))))
    (call-next-method totp-info encryption-key)))

(defmethod jfh-security:decrypt ((totp-info totp-info) &optional key)
  (let ((encryption-key (or key (coerce (jfh-security:fetch-key) '(vector (unsigned-byte 8)))))) ;; TODO can we get rid of COERCE?
    (call-next-method totp-info encryption-key)))

(defmethod save-totp-info ((totp-info totp-info))
  (jfh-store:save-object totp-info))

(defmethod get-totp-info ((application-user-id jfh-user:application-user-id))
  (let ((user-id (jfh-store:user-id application-user-id)))
    (jfh-store:make-instance* 'totp-info :user-id user-id)))

(defmethod tbnl:handle-request :after ((tbnl:*acceptor* jfh-web-server:ssl-client-cert-acceptor) (tbnl:*request* tbnl:request))
  (unless (jfh-web-server:can-skip-certificate-auth)
    (setf jfh-security:*key* (cl+ssl:certificate-fingerprint (tbnl:get-peer-ssl-certificate))))
  (when (next-method-p)
    (call-next-method)))

(defun make-hash-table-from-list (list)
  (let ((ht (make-hash-table :test #'eq :size 8)))
    (loop for item in list
          do
             (let* ((raw-value (cadr item))
                    (value (if (simple-vector-p raw-value)
                               (coerce raw-value '(simple-array (unsigned-byte 8) (*)))
                               raw-value)))
               (setf (gethash (car item) ht) value)))
    ht))

(defmethod initialize-instance :after ((webauthn-info webauthn-info) &key)
  "Initializations:
- Transform list to hash table"
  (let ((public-key #1=(slot-value webauthn-info '%public-key)))
    (when (eql 'cons (type-of public-key))
      (setf #1#
            (make-hash-table-from-list public-key)))))

(defmethod initialize-instance :after ((webauthn-info-readable webauthn-info-readable) &key)
  "Initializations:
- Transform hash table to k-v list"
  (let ((public-key-readable (if (slot-boundp webauthn-info-readable '%public-key-readable) #1=(slot-value webauthn-info-readable '%public-key-readable) nil)))
    (when (and public-key-readable (eql 'hash-table (type-of public-key-readable)))
      (setf #1#
            (loop for k being the hash-key
                    using (hash-value v) of public-key-readable
                  collect
                  (list k v))))))

(defmethod jfh-user:get-user-info ((user-login application-user-webauthn-credentials))
  "Search for user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-webauthn-credential-index-entry :where `(:user-credential-id ,(user-credential-id user-login)))
                          ))
    (jfh-store:make-instance* 'webauthn-info-readable :user-id (jfh-store:user-id user-index-entry))))

(defmethod jfh-user:save-application-user ((application-user webauthn-info-readable))
  "Input: webauthn-info-readable. Persist user's webauthn info."
  (jfh-store:save-object application-user))

(defmethod jfh-user:make-user-login-index-entry ((application-user webauthn-info-readable))
  "Input: webauthn-info-readable. Output: user credential-ID index entry."
  (make-instance 'user-webauthn-credential-index-entry
		 :user-credential-id (credential-id application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod jfh-user:save-index ((application-user webauthn-info-readable))
  (when (and
         (credential-id application-user)
         (plusp (length (credential-id application-user))))
    (jfh-store:save-object (jfh-user:make-user-login-index-entry application-user))))

(defmethod jfh-user:save-new-application-user ((application-user webauthn-info-readable))
  "Input: webauthn-info-readable. Persist user's webauthn info."
  (jfh-user:save-index application-user)
  (jfh-user:save-application-user application-user))
