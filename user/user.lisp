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

(defun user-entry->application-secure-user (application-user user-entry) ;; TODO convert to defmethod
  "Input: p-list. Output: application-secure-user."
  (make-instance 'application-secure-user
		 :user-id (user-id application-user)
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

(defmethod hash-password ((plaintext-password string) salt)
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core plaintext-password salt))

(defmethod hash-password ((plaintext-password simple-vector) salt)
  "Input: plaintext password. Output: Encrypted password (string)."
  (let ((string-password (format nil "~{~A~^ ~}" (coerce plaintext-password 'list))))
    (hash-password-core string-password salt)))

(defun hash-password-core (plaintext-password salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:pbkdf2-hash-password 
    (ironclad:ascii-string-to-byte-array plaintext-password)
    :salt (ironclad:ascii-string-to-byte-array salt)
    :iterations 100000)))
