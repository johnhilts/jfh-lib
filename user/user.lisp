(in-package #:jfh-user)

(defun make-application-user (user-id)
  "Constructor for application-user."
  (make-instance 'application-user :user-id user-id :user-login ""))

(defun make-application-secure-user (user-login user-password)
  "Constructor for application-secure-user."
  (make-instance 'application-secure-user :user-login user-login :user-password user-password))

(defun get-user-index-file-path (user-path-root &optional (user-identifier-class nil))
  (case user-identifier-class
    (application-user-fingerprint (format nil "~Auser-fingerprint-index.sexp" user-path-root))
    (t (format nil "~Auser-index.sexp" user-path-root))))

(defun get-user-fingerprint-index-file-path (user-path-root)
  (format nil "~Auser-fingerprint-index.sexp" user-path-root))

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

;; TODO remove next 2 lines
;; (defparameter *data-store-location* nil) ;; why is this defined here?? TODO: move to STORE
;; (setf *data-store-location* (make-instance 'jfh-store:data-store-location :settings-file-path "./" :user-path-root "./users/"))

(defun read-user-info (user-id file-name) ;; TODO: do we still need this?? using it in web-app
  "read user info from user-id/user.sexp The guid-like user ID is needed to find the folder."
  (let ((user-info (make-application-user user-id)))
    (jfh-store:read-complete-file (format nil "~A~A" (get-user-path user-info jfh-store:*data-store-location*) file-name))))

(defun get-user-info-OLD (user-login) ;; TODO: remove
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-login jfh-store:*data-store-location*))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defun get-secure-user-info-OLD (user-login) ;; TODO: remove
  "Search for secure user info in file system."
  (let* ((application-user (get-user-info user-login)))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defun hash-password (plaintext-password)
  "Input: plaintext password. Output: Encrypted password."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array plaintext-password))))
