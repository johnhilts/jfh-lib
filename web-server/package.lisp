(in-package #:cl-user)

(defpackage #:jfh-web-server
  (:use #:common-lisp)
  (:export
   #:make-web-configuration
   #:*web-application*
   #:web-application-shell
   #:stop-web-app
   #:web-configuration
   #:static-root
   #:add-static-path-map
   #:define-api-endpoint
   #:fetch-or-create-user-session
   #:verb
   #:web))
