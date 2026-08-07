(in-package #:cl-user)

(defpackage #:jfh-web-auth
  (:use #:common-lisp)
  (:export
   #:webauthn
   #:define-protected-page
   #:authenticated-user-id
   #:the-session-key
   #:signup-page
   #:login-page
   #:validate-signup-parameters
   #:establish-user-session
   #:get-authenticated-user
   #:validate-mfa-totp
   #:webauthn-register-start
   #:webauthn-register-finish
   #:webauthn-login-start
   #:webauthn-login-finish))
