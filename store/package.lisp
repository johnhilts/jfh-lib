(in-package #:cl-user)

(defpackage #:jfh-store
  (:use #:common-lisp)
  (:export
   #:*app-data-path*
   #:database-store
   #:host
   #:catalog
   #:label
   #:file-store
   #:path
   #:user-store
   #:user-data-store
   #:key
   #:user-index-store
   #:user-config-store
   #:data
   #:serialized-data
   #:config-data
   #:user-config-data
   #:user-index-data
   #:user-data
   #:user-data-large
   #:get-data-path
   #:get-data
   #:serialize-object->list
   #:save-data
   #:read-complete-file
   #:write-complete-file
   #:fetch-or-create-data
   #:make-instance-from-data))
