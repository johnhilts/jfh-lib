;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defun get-authenticated-user ()
  "Get the authenticated user from server session."
  (let ((the-session (tbnl:start-session)))
    (jfh-web-server::my-log (format nil "From G-A-U: ~A,~A~%" (tbnl:session-value 'jfh-web-server::the-client-key the-session) (tbnl:session-cookie-value the-session)))
    (tbnl:session-value 'jfh-web-auth:the-session-key the-session)))

(defun validate-signup-parameters (name user-login password confirm-password)
  "Validate the values used to signup a user."
  (flet ((exists (user-login)
           (jfh-user:get-secure-user-info (make-instance 'jfh-user:application-user-login :user-login user-login))))
    (let ((signup-validation-failure-reasons ()))
      (if (or
	   (zerop (length name))
	   (zerop (length user-login))
	   (zerop (length password))
	   (zerop (length confirm-password)))
          (push "Please enter all fields." signup-validation-failure-reasons))
      (progn
        (when (exists user-login)
          (push "User already exists; please login." signup-validation-failure-reasons))
        (when (not (string= password confirm-password))
          (push "Passwords don't match." signup-validation-failure-reasons)))
      (values
       (zerop (length signup-validation-failure-reasons))
       signup-validation-failure-reasons))))

(defun needs-mfa-check (user-id)
  (let* ((last-mfa-check (gethash user-id *mfa-checks* 'not-found))
         (mfa-check-not-found (eql 'not-found last-mfa-check))
         (last-mfa-check-expired (or
                                  mfa-check-not-found
                                  (>
                                   (- (get-universal-time) last-mfa-check)
                                   (* 60 10)))))
    (or
     mfa-check-not-found
     last-mfa-check-expired)))

(defun needs-mfa-setup (user-id)
  "Check whether user needs MFA setup. The check is based on whether the MFA key is populated; it defaults to an empty string, so check using LENGTH is safe."
  (let ((totp-info (get-totp-info (make-instance 'jfh-user:application-user-id :user-id user-id))))
    (or (not totp-info) (zerop (length (jfh-security:cipher totp-info))))))
