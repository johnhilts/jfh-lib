(in-package #:cl-user)

(defpackage #:jfh-store
  (:use #:common-lisp)
  (:export
   #:*app-data-path*
   #:data
   #:config-data
   #:user-config-data
   #:user-index-data
   #:user-data
   #:user-data-large
   #:make-instance*
   #:get-data
   #:save-object
   #:read-complete-file
   #:write-complete-file
   #:fetch-or-create-data
   #:make-instance-with-partial-data))
