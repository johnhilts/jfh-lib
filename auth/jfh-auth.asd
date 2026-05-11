(cl:in-package #:asdf-user)

(defsystem #:jfh-auth
  :description "Authentication and authorization features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:jfh-configuration #:cffi #:cl+ssl #:cl-ppcre #:jfh-security #:cl-base32 #:cl-one-time-passwords #:ironclad #:cbor #:cl-json)
  :components ((:file package)
               (:file c-wrapper)
               (:file auth-protocol)
               (:file auth-implementation)
               (:file mfa)
               (:file auth)
               (:file certificate-util)))
