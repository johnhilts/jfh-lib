(in-package #:cl-user)

(defpackage #:jfh-user
  (:use #:common-lisp)
  (:export
   #:application-user
   #:application-meta-user
   #:application-secure-user
   #:application-user-id
   #:application-user-login
   #:application-user-api-key
   #:application-user-fingerprint
   #:user-id
   #:user-login
   #:user-password
   #:user-api-key
   #:user-fingerprint
   #:make-application-user
   #:read-user-info
   #:save-user
   #:save-new-application-user
   #:save-application-user
   #:get-user-info
   #:get-secure-user-info
   #:hash-password))
