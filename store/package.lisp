(in-package #:cl-user)

(defpackage #:jfh-store
  (:use #:common-lisp)
  (:export
   #:*store-root-folder*
   #:store-object
   #:store-data
   #:user-store-object
   #:user-index-store-object
   #:get-data
   #:save-user-data
   #:save-index
   #:save-data
   #:serialize-object->list
   #:serialize-object
   #:make-data-store
   #:make-instance-from-data-store
   #:write-instance-to-data-store
   #:data-store-location
   #:settings-file-path
   #:user-path-root
   #:get-data-store-location
   #:fetch-or-create-data
   #:write-complete-file
   #:read-complete-file
   #:*data-path*
   #:*index-path*
   #:*data-store-location*))
