(cl:in-package #:asdf-user)

(defsystem #:jfh-security
  :description "Security interface library."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:ironclad #:cl-base64)
  :components ((:file package)
               (:file security-protocol)
               (:file security-implementation)))
