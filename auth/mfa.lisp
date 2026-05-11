;;;; functions for auth related concerns, with a focus on MFA. 
(cl:in-package #:jfh-auth)

;; TODO need to encrypt the keys
(defparameter *totp-keys* (make-hash-table :test #'equal))

(defun parse-to-integer-or-default (number &optional (default 0))
  "Handle PARSE-INTEGER failure by return 0"
  (handler-case
      (parse-integer number)
    (error () (return-from parse-to-integer-or-default default))))

(defun refresh-mfa-expiration (user-id mfa-type &optional (time (get-universal-time)))
  "Refresh time of lastest MFA check"
  (let ((mfa-checks-by-user (if (eql mfa-type 'webauthn-mfa) *webauthn-checks* *totp-checks*)))
    (setf (gethash user-id mfa-checks-by-user) time)))

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
    (format nil "otpauth://totp/~A:~A?secret=~A&issuer=~A" base-url user-id b32-encoded-secret base-url)))

(defun base32-encode-hex-string (hex-string)
  "Base 32 encode hex string"
  (base32:bytes-to-base32 (coerce (jfh-utility::hex-string-to-base10-list hex-string) 'vector)))
