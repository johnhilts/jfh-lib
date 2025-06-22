(in-package #:jfh-user)

(defun make-application-user (user-id)
  "Constructor for application-user."
  (make-instance 'application-user :user-id user-id :user-login ""))

(defun user-entry->application-user (user-entry)
  "Input: p-list. Output: application-user."
  (make-instance 'application-meta-user
		 :user-id (getf user-entry :user-id)
		 :user-login (getf user-entry :user-login)
		 :create-date (getf user-entry :create-date)
		 :disable (getf user-entry :disable)))

(defun user-entry->application-secure-user (application-user user-entry) ;; TODO convert to defmethod; NOTE: it's possible we're not even using this
  "Input: p-list. Output: application-secure-user."
  (make-instance 'application-secure-user
		 :user-id (jfh-store:user-id application-user)
		 :user-login (user-login application-user)
		 :user-password (getf user-entry :user-password)))

(defun hash-password-core-OLD (plaintext-password)
  "Input: plaintext password. Output: Encrypted password (string)."
  (let ((cipher
          (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence
            :sha256
            (ironclad:ascii-string-to-byte-array plaintext-password)))))
    (coerce
     (loop for char across cipher
           collect char)
     'simple-string)))

(defparameter *hard-coded-salt* #(147 150 187 231 142 93 174 220 65 175 19 50 253 229 47 56))

(defmethod hash-password ((application-password application-password))
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core (user-password application-password) (salt application-password)))

(defmethod hash-password ((application-fingerprint application-fingerprint))
  "Input: plaintext password. Output: Encrypted password (string)."
  (let ((string-password (format nil "~{~A~^ ~}" (coerce (user-fingerprint application-fingerprint) 'list))))
    (hash-password-core string-password *hard-coded-salt*)))

(defmethod hash-password ((application-api-key application-api-key))
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core (user-api-key application-api-key) *hard-coded-salt*))

(defun hash-password-core (plaintext-password salt)
  (let ((salt-string (format nil "~{~A~^ ~}" (coerce salt 'list))))
    (ironclad:byte-array-to-hex-string
     (ironclad:pbkdf2-hash-password 
      (ironclad:ascii-string-to-byte-array plaintext-password)
      :salt (ironclad:ascii-string-to-byte-array salt-string)
      :iterations 100000))))
