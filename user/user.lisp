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

(defun hash-password (plaintext-password)
  "Input: plaintext password. Output: Encrypted password."
  (let* ((string-password (if (typep plaintext-password 'string) plaintext-password (format nil "~{~A~^ ~}" (coerce plaintext-password 'list))))
         (cipher
           (ironclad:byte-array-to-hex-string
            (ironclad:digest-sequence
             :sha256
             (ironclad:ascii-string-to-byte-array string-password)))))
    (coerce
     (loop for char across cipher
           collect char)
     'simple-string)))
