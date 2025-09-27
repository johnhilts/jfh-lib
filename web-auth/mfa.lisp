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

(defun validate-mfa-totp (user-id input-totp &optional (minute-tolerance 0))
  "Validate TOTP for previous, current, and next minute."
  (let ((parsed-totp (parse-to-integer-or-default input-totp))
        (repeats (1+ (* 2 minute-tolerance))))
    (flet ((get-valid-totps ()
             (loop for i = (* -1 minute-tolerance 60) then (incf i 60) repeat repeats
                   collect
                   (totp:totp (get-mfa-key user-id) i))))
      (find parsed-totp (get-valid-totps) :test #'=))))

(defun refresh-mfa-expiration (user-id &optional (time (get-universal-time)))
  "Refresh time of lastest MFA check"
  (setf (gethash user-id *mfa-checks*) time))

;; TODO might be good to have MFA actions that specialize on types such as a "TOTP"
(defun generate-mfa-secret ()
  "Generate a 20 byte secret and output as hex string suitable for persistence."
  (jfh-utility::byte-array-to-hex-string (ironclad:random-data 20)))

(defun print-totp-url (base-url user-id totp)
  "Print a TOTP secret URL suitable for an authenticator app.
The format is:
otpauth://totp/user@example.com?secret=jbswy3dpehpk3pxpjbswy3dpehpk3pxp
or
otpauth://totp/Issuer:AccountName?secret=BASE32SECRET&issuer=Issuer
or
otpauth://totp/test.com:me@here.com?secret=YOURSECRET&issuer=test.com"
  (let ((b32-encoded-secret (base32:bytes-to-base32 (coerce (jfh-utility::hex-string-to-base10-list totp) 'vector))))
    ;; (format nil "otpauth://totp/~A:~A?secret=~A&issuer=~A" base-url user-id b32-encoded-secret base-url)
    b32-encoded-secret))
