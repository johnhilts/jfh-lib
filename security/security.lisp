(in-package #:jfh-security)

(defparameter *chasi-encryption-keys* (make-hash-table :test #'equal))

(defun generate-random-iv ()
  "Generate a random initialization vector (IV) for AES."
  (ironclad:random-data 16))

(defun fetch-key (user-id)
  "Fetch key from cache"
  (gethash user-id *chasi-encryption-keys*))
