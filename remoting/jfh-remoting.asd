(cl:in-package #:asdf-user)

(defsystem #:jfh-remoting
  :description "Remoting features, such as swank."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:swank)
  :components ((:file package)
               (:file remoting-protocol)
               (:file remoting-implementation)))
