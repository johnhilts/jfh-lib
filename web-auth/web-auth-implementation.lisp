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

(defparameter *mfa-checks* (make-hash-table)) ;; track MFA checks by user

(defmethod jfh-web-server:prompt-mfa ((tbnl:*request* tbnl:request) user-id)
  "Redirect to MFA prompt. The conditions are: 1. No recent MFA check."
  (when (needs-mfa-check user-id)
    (tbnl:redirect (format nil "/prompt-mfa?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  ;; sliding MFA expiration
  (setf (gethash user-id *mfa-checks*) (get-universal-time)))
