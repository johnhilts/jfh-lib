(cl:in-package #:asdf-user)

(defsystem #:jfh-user
  :description "User support features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:ironclad)
  :components ((:file package)
               (:file user-protocol)
               (:file user)
               (:file user-implementation)))
