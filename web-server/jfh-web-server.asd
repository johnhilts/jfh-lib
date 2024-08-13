(cl:in-package #:asdf-user)

(defsystem #:jfh-web-server
  :description "Web server features."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot)
  :components ((:file package)
               (:file macros)
               (:file web-server-protocol)
               (:file web-server)
               (:file web-server-implementation)))
