(cl:in-package #:asdf-user)

(defsystem #:jfh-configuration
  :description "Configuration settings."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot)
  :components ((:file package)
               (:file configuration-protocol)
               (:file configuration)
               (:file configuration-implementation)))
