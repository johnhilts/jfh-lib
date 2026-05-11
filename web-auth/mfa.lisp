;;;; functions for auth related concerns, with a focus on MFA. 
(cl:in-package #:jfh-web-auth)

(defun get-totp-key (user-id)
  "Just for abstracting how we get the TOTP key"
  (let* ((cached-totp-key (gethash user-id jfh-auth:*totp-keys*))
         (totp-info (get-totp-info (make-instance 'jfh-user:application-user-id :user-id user-id)))
         (totp-key (or cached-totp-key (if totp-info (jfh-security:decrypt totp-info) ""))))
    (setf
     (gethash user-id jfh-auth:*totp-keys*)
     totp-key)))

(defun get-valid-totps (user-id minute-tolerance repeats)
  (let ((totp-key (get-totp-key user-id)))
    (loop for i = (* -1 minute-tolerance 60) then (incf i 60) repeat repeats
          collect
          (totp:totp totp-key i))))

(defun validate-mfa-totp (user-id input-totp &key (minute-tolerance 0))
  "Validate TOTP for previous, current, and next minute."
  (let ((parsed-totp (jfh-auth:parse-to-integer-or-default input-totp))
        (repeats (1+ (* 2 minute-tolerance))))
    (let ((valid-totps (get-valid-totps user-id minute-tolerance repeats)))
      (format t "~&Valid TOTPs: ~A~%" valid-totps)
      (find parsed-totp valid-totps :test #'=))))

