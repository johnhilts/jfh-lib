(in-package #:cl-user)

(defpackage #:jfh-store
  (:use #:common-lisp)
  (:export
   #:*app-data-path*
   #:data
   #:config-data ;; remove
   #:configuration
   #:user-config-data
   #:user-index
   #:user-settings
   #:user-data
   #:user-id
   #:data-id
   #:user-data-large
   #:user-login-index
   #:user-fingerprint-index
   #:user-apikey-index
   #:make-instance*
   #:make-instance-list
   #:get-data
   #:save-object
   #:delete-object
   #:save-index
   #:read-complete-file
   #:write-complete-file
   #:fetch-or-create-data
   #:get-next-data-id
   #:*serialized-fields*))
