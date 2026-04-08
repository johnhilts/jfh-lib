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

(defmethod jfh-web-server:mfa-enabled-schemes ((configuration jfh-web-server:web-configuration))
  "Determine whether MFA is enabled, and what types. Return list of supported and enabled MFA schemes."
  (remove-if-not #'identity
                 (list
                  (if (jfh-web-server:enable-totp configuration) 'totp-mfa nil) 
                  (if (jfh-web-server:enable-webauthn configuration) 'webauthn-mfa nil))))

(defparameter *mfa-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defparameter *totp-checks* (make-hash-table :test #'equal) "Track TOTP checks by user")

(defparameter *webauthn-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defun mfa-check (user-id mfa-checks-by-user test-type)
  "Generalized logic for determining if a user needs to be prompted for MFA."
  (let* ((last-mfa-check (gethash user-id mfa-checks-by-user 'not-found))
         (mfa-check-not-found (eql 'not-found last-mfa-check))
         (last-mfa-check-expired (or
                                  mfa-check-not-found
                                  (let ((test-threshold (mfa-test-threshold test-type)))
                                    (case test-type
                                      (mfa-time-test
                                       (>
                                        (- (get-universal-time) last-mfa-check)
                                        test-threshold))
                                      (mfa-count-test
                                       (> last-mfa-check test-threshold)))))))
    (or
     mfa-check-not-found
     last-mfa-check-expired)))

(defun mfa-test-type (enabled-mfa-schemes mfa-type)
  "Determine MFA test type by number of enabled MFA checks. Assuming this isn't called when no MFA checks are enabled."
  (case (length enabled-mfa-schemes)
    (1
     'mfa-time-test)
    (otherwise
     (case mfa-type
       (totp-mfa
        'mfa-count-test)
       (webauthn-mfa
        'mfa-time-test)))))

(defun mfa-test-threshold (test-type)
  "Get test threshold by test type."
   (if (eq 'mfa-time-test test-type) (* 60 100) 20))

(defun needs-totp-check (user-id enabled-mfa-schemes)
  (let ((test-type (mfa-test-type enabled-mfa-schemes 'totp-mfa)))
    (mfa-check user-id *totp-checks* test-type)))

(defun needs-webauthn-check (user-id enabled-mfa-schemes)
  (let ((test-type (mfa-test-type enabled-mfa-schemes 'webauthn-mfa)))
    (mfa-check user-id *webauthn-checks* test-type)))

(defmethod jfh-web-server:need-mfa-check ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Determine whether the given user ID needs an MFA check, and what types. Return list of MFA schemes to use in an MFA check."
  (let ((mfa-setup-in-progress (or
                                (search "totp-setup" (tbnl:script-name tbnl:*request*))
                                (search "b-registration" (tbnl:script-name tbnl:*request*))))
        (mfa-in-progress (or
                          (search "-mfa" (tbnl:script-name tbnl:*request*))
                          (search "-totp" (tbnl:script-name tbnl:*request*))
                          (search "webauthn" (tbnl:script-name tbnl:*request*))
                          (search "biometrics" (tbnl:script-name tbnl:*request*))))
        (mfa-can-skip (search "/styles.css" (tbnl:script-name tbnl:*request*))))
    (when (and
           user-id
           enabled-mfa-schemes
           (not mfa-setup-in-progress)
           (not mfa-in-progress)
           (not mfa-can-skip))
      (remove-if-not #'identity
                     (list 
                      (if (and
                           (member 'webauthn-mfa enabled-mfa-schemes)
                           (needs-webauthn-check user-id enabled-mfa-schemes)) 
                          'webauthn-mfa
                          'nil)
                      (if (and 
                           (member 'totp-mfa enabled-mfa-schemes)
                           (needs-totp-check user-id enabled-mfa-schemes))
                          'totp-mfa
                          nil))))))

(defun renew-mfa-check (user-id enabled-mfa-schemes mfa-checks mfa-type)
  "Renew mfa-check."  
  (case (mfa-test-type enabled-mfa-schemes mfa-type)
    (mfa-time-test
     ;; sliding expiration time
     (setf (gethash user-id mfa-checks) (get-universal-time)))
    (mfa-count-test
     (incf (gethash user-id mfa-checks)))))

(defmethod prompt-totp ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Redirect to TOTP prompt. The conditions are: 1. No recent MFA check."
  (if (needs-totp-setup user-id)
      (tbnl:redirect (format nil "/totp-setup?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*))))
      (tbnl:redirect (format nil "/prompt-totp?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  (renew-mfa-check user-id enabled-mfa-schemes *totp-checks* 'totp-mfa))
  
(defmethod prompt-webauthn ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Redirect to WebAuthN prompt. The conditions are: 1. No recent MFA check."
  (if (needs-webauthn-setup user-id)
      (tbnl:redirect (format nil "/b-registration?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*))))
      (tbnl:redirect (format nil "/prompt-webauthn?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  (renew-mfa-check user-id enabled-mfa-schemes *webauthn-checks* 'webauthn-mfa))

(defmethod jfh-web-server:prompt-mfa ((tbnl:*request* tbnl:request) user-id need-mfa-check enabled-mfa-schemes)
  (cond
    ((member 'totp-mfa need-mfa-check)
     (prompt-totp tbnl:*request* user-id enabled-mfa-schemes))
    ((member 'webauthn-mfa need-mfa-check)
     (prompt-webauthn tbnl:*request* user-id enabled-mfa-schemes))))

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

(defmethod get-webauthn-info ((application-user-id jfh-user:application-user-id))
  (let ((user-id (jfh-store:user-id application-user-id)))
    (jfh-store:make-instance* 'webauthn-info-readable :user-id user-id)))

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
