(cl:in-package #:asdf-user)

(defsystem #:jfh-store
  :description "Data store."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:jfh-utility #:closer-mop)
  :components ((:file package)
               (:file store-protocol)
               (:file file-io)
               (:file store-implementation)))
