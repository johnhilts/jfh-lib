(in-package #:jfh-security)

(defclass aes ()
  ((%plaintext
    :reader plaintext
    :initarg :plaintext)
   (%cipher
    :reader cipher
    :initarg :cipher
    :initform "")
   (%iv
    :reader iv
    :initarg :iv)))

(defgeneric encrypt (obj &optional key))

(defgeneric decrypt (obj &optional key))
