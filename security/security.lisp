(in-package #:jfh-security)

(defun generate-random-iv ()
  "Generate a random initialization vector (IV) for AES."
  (ironclad:random-data 16))

(defun fetch-key (user-id)
  "Fetch key from cache"
  (jfh-security:get-encryption-key *chasi-configuration* (make-instance 'jfh-user:application-user-api-key :user-api-key (jfh-user:user-fingerprint user-identifier))))
