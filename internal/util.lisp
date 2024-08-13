(in-package #:cl-user)
(defpackage #:jfh/internal/utilities
  (:use #:common-lisp))

(in-package #:jfh/internal/utilities)

(defun list-lisp-files-for-asd ()
  "Useful when writing asdf files. Meant to be used in current folder."
  (format nil "~%~{(:file ~A)~%~}"
          (mapcar (lambda (pn)
                    (format nil "~A" (pathname-name pn)))
                  (remove-if
                   (lambda (pn)
                     (char=
                      #\.
                      (char (namestring pn) 0)))
                   (remove-if-not
                    (lambda (pn)
                      (string-equal
                       "lisp"
                       (pathname-type pn)))
                    (directory "*.lisp"))))))
