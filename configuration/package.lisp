(in-package #:cl-user)

(defpackage #:jfh-configuration
  (:use #:common-lisp)
  (:export
   #:application
   #:application-configuration
   #:swank-port
   #:swank-interface
   #:settings-file-path
   #:user-path-root
   #:bind-configuration
   #:get-configuration))
