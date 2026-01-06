(cl:in-package #:asdf-user)

(defsystem #:jfh-web-auth
  :description "Web authentication and authorization features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:jfh-security #:jfh-user #:jfh-web-server #:cl-base32 #:cl-one-time-passwords #:ironclad)
  :components ((:file package)
               (:file macros)
               (:file web-auth-protocol)
               (:file web-auth)
               (:file mfa)
               (:file web-auth-implementation)
               (:file certificate-util)
               (:file pages)))
