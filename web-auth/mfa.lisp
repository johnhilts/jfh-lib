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
         (2-minutes-ago-totp (totp:totp mfa-key -120))
         (1-minute-ago-totp (totp:totp mfa-key -60))
         (current-totp (totp:totp mfa-key))
         (1-minute-from-now-totp (totp:totp mfa-key 60))
         (2-minutes-from-now-totp (totp:totp mfa-key 120))
         (valid-totps (list 2-minutes-ago-totp 1-minute-ago-totp current-totp 1-minute-from-now-totp 2-minutes-from-now-totp))
         (parsed-totp (parse-to-integer-or-default input-totp)))
    (some
     (lambda (e)
       ;; (format t "~&[MFA] comparing ~D with ~D~%" e parsed-totp)
       (=
        parsed-totp
        e))
     valid-totps)))

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
