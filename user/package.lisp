(in-package #:cl-user)

(defpackage #:jfh-user
  (:use #:common-lisp)
  (:export
   #:application-user
   #:application-meta-user
   #:application-secure-user
   #:user-id
   #:user-login
   #:user-password
   #:make-application-user
;;   #:read-user-info
   #:save-user
   #:save-new-application-user
   #:save-application-user
   #:get-user-info
   #:get-secure-user-info
   #:hash-password ;; do we need to export this?
   #:hash-user-password
   ;; #:get-user-path
   ))
