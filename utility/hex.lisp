;;;; hex utility
(in-package #:jfh-utility)

;; 65fcaf48b497fd06e9db800fab4dae277b08fe77
;; (code-char 101) #\e (char-code #\e) 101
;; (code-char 65) #\A
;; (parse-integer "65" :radix 16) 101

(defun hex-string-to-base10-list-OLD (hex)
  "Take a long hex string such as \"65fcaf48b497fd06e9db800fab4dae277b08fe77\" and convert it to a list of base 10 integers"
  (with-input-from-string (input hex)
    (remove-if
     #'null
     (loop for char = (read-char input nil nil) then (read-char input nil nil)
           for i = 1 then (incf i)
           for acc = char then (if acc (list acc char) char)
           while char
           collect
           (when (zerop (mod i 2))
             (prog1
                 (format nil "~A"
                         (parse-integer
                          (reduce
                           (lambda (acc cur)
                             (concatenate 'string acc (string cur)))
                           (format nil "~{~A~}" acc)
                           :initial-value "")
                          :radix 16))
               (setf acc nil)))))))

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
