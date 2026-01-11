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

(defgeneric get-encryption-key (chasi-configuration user-api-key))

(defgeneric cache-encryption-key (user-identifier))

(defgeneric encrypt (obj &optional key))

(defgeneric decrypt (obj &optional key))
