(asdf:defsystem #:jfh-user-tests
  :description "Specs for jfh-user library"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:jfh-store #:jfh-testing #:jfh-user)
  :components ((:file "package")
               (:file "jfh-user-tests")))
