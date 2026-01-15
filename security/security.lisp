(in-package #:jfh-security)

(defparameter *chasi-encryption-keys* (make-hash-table :test #'equalp))

(defun generate-random-iv ()
  "Generate a random initialization vector (IV) for AES."
  (ironclad:random-data 16))

(defun fetch-key ()
  "Fetch key from cache"
  (let ((fingerprint (cl+ssl:certificate-fingerprint (tbnl:get-peer-ssl-certificate))))
    (gethash fingerprint *chasi-encryption-keys*)))
