(in-package #:cl-user)

(defpackage #:jfh-web-server
  (:use #:common-lisp)
  (:export
   #:make-web-configuration
   #:ssl-port
   #:*web-application*
   #:web-application-shell
   #:stop-web-app
   #:web-configuration
   #:static-root
   #:add-static-path-map
   #:define-api-endpoint
   #:get-form-object
   #:getv
   #:fetch-or-create-user-session
   #:site-origin
   #:site-display-name
   #:site-registrable-domain
   #:mfa-enabled-schemes
   #:need-mfa-check
   #:prompt-mfa
   #:can-skip-certificate-auth
   #:ssl-client-cert-acceptor
   #:verb
   #:web))
