(in-package #:cl-user)

(defpackage #:jfh-web-auth
  (:use #:common-lisp)
  ;; (:local-nicknames (#:web #:jfh-web-core))
  (:export
   ;; #:auth
   #:webauthn
   #:define-protected-page
   #:authenticated-user-id
   ;; #:use-web-auth
   #:the-session-key
   ;; #:show-auth-failure
   ;; #:get-certificate-fingerprint-from-file
   ;; #:logout-page
   #:signup-page
   #:login-page
   #:validate-signup-parameters
   #:establish-user-session
   #:get-authenticated-user
   ;; mfa
   ; #:enable-webauthn ;; commenting out bc not sure if they need to be exported
   ; #:enable-totp ;; commenting out bc not sure if they need to be exported
   #:validate-mfa-totp
   ;; #:refresh-mfa-expiration
   ;; #:webauthn-mfa
   ;; #:totp-mfa
   ;; #:prompt-totp
   ;; #:prompt-webauthn
   ;; #:print-totp-url
   ;; #:totp-info
   ;; #:get-totp-info
   ;; #:save-totp-info
   ;; #:base32-encode-hex-string
   ;; #:get-webauthn-info
   #:webauthn-register-start
   #:webauthn-register-finish
   #:webauthn-login-start
   #:webauthn-login-finish
   ;; #:webauthn-challenge
   ;; #:webauthn-info-readable
   ;; #:webauthn-info
   ))
