(in-package #:cl-user)

(defpackage #:jfh-store
  (:use #:common-lisp)
  (:export
   #:make-instance-from-data-store
   #:write-instance-to-data-store
   #:data-store-location
   #:settings-file-path
   #:user-path-root
   #:get-data-store-location
   #:fetch-or-create-data
   #:write-complete-file
   #:read-complete-file
   #:*data-store-location*))
