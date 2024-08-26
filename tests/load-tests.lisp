(cl:in-package #:cl-user)

(defun load-local-tests ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/jfh/testing/")
  (push #p"/home/jfh/code/lisp/source/jfh/testing/" asdf:*central-registry*)
  (asdf:load-system "jfh-testing")
  (print "jfh-testing loaded")
  
  (swank:set-default-directory "/home/jfh/code/lisp/source/jfh/tests/jfh-user/")
  (push #p"/home/jfh/code/lisp/source/jfh/tests/jfh-user/" asdf:*central-registry*)
  (asdf:load-system "jfh-user-tests")
  (print "jfh-user-tests loaded")
  t)
