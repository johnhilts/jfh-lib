;;;; hex utility
(in-package #:jfh-utility)

(defun ensure-proper-hex-string (hex)
  (unless (evenp (length hex))
    (cerror "Hex string must have an even number of characters." hex)
    (format t "~&Type a hex string with an even number of characters: ")
    (setq hex (read))
    (ensure-proper-hex-string hex))
  hex)

(defun hex-string-to-base10-list (hex)
  "Convert a hex string like \"65fcaf48...\" into a list of base-10 integers."
  (let ((hex (ensure-proper-hex-string hex)))
    (loop for i from 0 below (length hex) by 2
          collect (parse-integer hex :start i :end (+ i 2) :radix 16))))

(defun byte-array-to-hex-string (bytes)
  "Take an array of base 10 integers and produce a hex string;
Example: #(9 10 11 255) produces \"090a0bff\""
  (with-output-to-string (out)
    (map nil (lambda (byte)
               (format out "~(~2,'0X~)" byte))
         bytes)))
