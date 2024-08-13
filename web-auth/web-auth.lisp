;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defparameter *web-auth-pages* nil)

(defun get-authenticated-user ()
  "Get the authenticated user from server session."
  ;; (gethash (tbnl:session-value 'the-session) (session-user-map *web-auth-pages*))
  (let ((the-session (tbnl:start-session)))
    (jfh-web-server::my-log (format nil "From G-A-U: ~A,~A~%" (tbnl:session-value 'jfh-web-server::the-client-key the-session) (tbnl:session-cookie-value the-session)))
    (tbnl:session-value 'jfh-web-server::the-session the-session)))

(defun validate-signup-parameters (name user-login password confirm-password)
  "Validate the values used to signup a user."
  (flet ((exists (user-login)
           (funcall (find-user-info *web-auth-pages*) user-login)))
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
