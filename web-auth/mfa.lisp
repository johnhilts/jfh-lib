;;;; functions for auth related concerns, with a focus on MFA. 
(cl:in-package #:jfh-web-auth)

;; TODO this will need to be input and stored securely in APPLICATION-SECURE-USER
(defparameter *mfa-key* "48656C6C6F21DEADBEEF48656C6C6F21DEADBEEF") ;; from an example

;; TODO need to encrypt the keys
(defparameter *mfa-keys* (make-hash-table :test #'equal))

(defun get-mfa-key (user-id)
  "Just for abstracting how we get the MFA key"
  (setf
   (gethash user-id *mfa-keys*)
   *mfa-key*))

(defun parse-to-integer-or-default (number &optional (default 0))
  "Handle PARSE-INTEGER failure by return 0"
  (handler-case
      (parse-integer number)
    (error () (return-from parse-to-integer-or-default default))))

(defun validate-mfa-totp (user-id input-totp)
  "Validate TOTP for previous, current, and next minute."
  (let* ((mfa-key (get-mfa-key user-id))
         (1-minute-ago-totp (totp:totp mfa-key -60))
         (current-totp (totp:totp mfa-key))
         (1-minute-from-now-totp (totp:totp mfa-key 60))
         (valid-totps (list 1-minute-ago-totp current-totp 1-minute-from-now-totp)))
    (some
     (lambda (e)
       (=
        (parse-to-integer-or-default input-totp)
        e))
     valid-totps)))

(defun refresh-mfa-expiration (user-id &optional (time (get-universal-time)))
  "Refresh time of lastest MFA check"
  (setf (gethash user-id *mfa-checks*) time))

