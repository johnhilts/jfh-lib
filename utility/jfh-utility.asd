(cl:in-package #:asdf-user)

(defsystem #:jfh-utility
  :description "Useful general functions."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file package)
               (:file guid)
               (:file string)))
