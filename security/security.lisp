(in-package #:jfh-security)

(defparameter *chasi-encryption-keys* (make-hash-table :test #'equalp))

(defparameter *key* nil)

(defun generate-random-iv ()
  "Generate a random initialization vector (IV) for AES."
  (ironclad:random-data 16))

(defun fetch-key ()
  "Fetch key from cache"
  (gethash *key* *chasi-encryption-keys*))
