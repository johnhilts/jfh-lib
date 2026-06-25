(cl:in-package #:asdf-user)

(defsystem #:jfh-validation
  :description "Validation utilities."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:swank #:jfh-configuration)
  :components ((:file package)
               (:file validation-protocol)
               (:file validation)
               (:file validation-implementation)))
