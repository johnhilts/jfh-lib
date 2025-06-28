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

(defparameter *hard-coded-salt* #A((16) (unsigned-byte 8) 147 150 187 231 142 93 174 220 65 175 19 50 253 229 47 56)
              "Note: to produce this value, I used (LET ((*PRINT-READABLY* T)) (PRIN1 (IRONCLAD:MAKE-RANDOM-SALT))) which adds the type annotations")

(defun hash-password-core (password salt)
  (let ((string-password (if (stringp password) password (format nil "~{~A~^ ~}" (coerce password 'list)))))
    (ironclad:byte-array-to-hex-string
     (ironclad:pbkdf2-hash-password 
      (ironclad:ascii-string-to-byte-array string-password)
      :salt salt
      :iterations 100000))))

(defmethod hash-password ((application-password application-password))
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core (user-password application-password) (salt application-password)))

(defmethod hash-password ((application-fingerprint application-fingerprint))
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core (user-fingerprint application-fingerprint) *hard-coded-salt*))

(defmethod hash-password ((application-api-key application-api-key))
  "Input: plaintext password. Output: Encrypted password (string)."
  (hash-password-core (user-api-key application-api-key) *hard-coded-salt*))
