(in-package #:cl-user)

(defpackage #:jfh-configuration
  (:use #:common-lisp)
  (:export
   #:application-configuration ;; TODO: needed?
   #:*application-configuration*
   #:swank-port
   #:swank-interface
   #:settings-file-path
   #:user-path-root))
