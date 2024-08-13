(cl:in-package #:asdf-user)

(defsystem #:jfh-web-auth
  :description "Web authentication and authorization features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:jfh-user #:jfh-web-server)
  :components ((:file package)
               (:file macros)
               (:file web-auth-protocol)
               (:file web-auth)
               (:file web-auth-implementation)
               (:file pages)))
