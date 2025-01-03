(in-package #:cl-user)

(defpackage #:jfh-web-auth
  (:use #:common-lisp)
  ;; (:local-nicknames (#:web #:jfh-web-core))
  (:export
   #:define-protected-page
   #:authenticated-user
   ;; #:use-web-auth
   #:session-user-map ;; TODO - remove
   #:the-session-key
   #:show-auth-failure
   ;; #:logout-page
   #:signup-page
   #:login-page
   #:validate-signup-parameters
   #:establish-user-session
   #:get-authenticated-user))
