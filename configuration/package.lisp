(in-package #:cl-user)

(defpackage #:jfh-configuration
  (:use #:common-lisp)
  (:export
   #:application
   #:app ;; TODO this should probably be consolidated with application
   #:application-configuration
   #:swank-port
   #:swank-interface
   #:settings-file-path
   #:user-path-root
   #:enable-console-logging
   #:bind-configuration
   #:rebind-configuration
   #:get-configuration))
