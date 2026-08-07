(in-package #:cl-user)

(defpackage #:jfh-auth
  (:use #:common-lisp)
  (:export
   #:auth
   #:auth-configuration
   #:*auth-configuration*
   #:*webauthn-configuration*
   #:*session-user-map*
   #:on-auth-success
   #:on-auth-failure
   #:get-certificate-fingerprint-from-file
   #:enable-webauthn
   #:enable-totp
   #:generate-totp-secret
   #:refresh-mfa-expiration
   #:webauthn-mfa
   #:totp-mfa
   #:prompt-totp
   #:prompt-webauthn
   #:print-totp-url
   #:totp-info
   #:get-totp-info
   #:save-totp-info
   #:base32-encode-hex-string
   #:needs-totp-check
   #:needs-webauthn-check
   #:webauthn-challenge
   #:webauthn-info-readable
   #:webauthn-info
   #:user-webauthn-credential-index
   #:credential-id
   #:renew-mfa-check
   #:timeout
   #:challenge
   #:verify-es256
   #:public-key
   #:sign-count
   #:public-key-readable
   #:*mfa-checks*
   #:*totp-keys*
   #:parse-to-integer-or-default))
