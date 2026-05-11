(cl:in-package #:asdf-user)

(defsystem #:jfh-web-auth
  :description "Web authentication and authorization features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:cl+ssl #:hunchentoot #:jfh-security #:jfh-auth #:jfh-user #:jfh-web-server #:cl-base32 #:cl-one-time-passwords #:ironclad #:cbor #:cl-json)
  :components ((:file package)
               (:file macros)
               (:file web-auth-protocol)
               (:file web-auth-implementation)
               (:file mfa)
               (:file web-auth)
               (:file biometrics-util)
               (:file pages)))
