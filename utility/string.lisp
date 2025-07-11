;;;; string utility
(in-package #:jfh-utility)

(defun string-starts-with (search string)
  "Does the beginning of string match the search string?"
  (search search string :start2 0 :end2 (length search)))

(defun join (strings &optional delimiter)
  (let ((format-string (if delimiter (format nil "~~{~~A~~^~A~~}" delimiter) "~{~A~}")))
    (format nil format-string strings)))
