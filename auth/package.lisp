(in-package #:cl-user)

(defpackage #:jfh-auth
  (:use #:common-lisp)
  ;; (:local-nicknames (#:web #:jfh-web-core))
  (:export
   #:auth
   #:auth-configuration
   #:*auth-configuration*
   #:*webauthn-configuration*
   ;; #:webauthn
   ;; #:authenticated-user-id
   #:*session-user-map*
   ;; #:the-session-key
   #:on-auth-success
   #:on-auth-failure
   #:get-certificate-fingerprint-from-file
   #:validate-signup-parameters ;; TODO - remove
   ;; #:establish-user-session
   ;; #:get-authenticated-user
   ;; mfa
   #:enable-webauthn
   #:enable-totp
   ;;#:validate-mfa-totp
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
   ;; #:get-webauthn-info
   #:needs-webauthn-check
   #:webauthn-challenge
   #:webauthn-info-readable
   #:webauthn-info
   #:user-webauthn-credential-index-entry ;; TODO remove
   #:user-webauthn-credential-index
   #:credential-id
   #:clean-return-url ; TODO remove!
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
