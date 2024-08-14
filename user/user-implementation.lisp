(in-package #:jfh-user)

(defmethod initialize-instance :after ((application-secure-user application-secure-user) &key)
  "Initializations:
- Encrypt the user password. This is meant to prevent the plain text password from being in memory.
- Set the User ID to a unique ID."
  (let ((user-id #1=(slot-value application-secure-user '%user-id))
        (password #2=(slot-value application-secure-user '%user-password)))
    (when (zerop (length user-id))
      (setf #1# (jfh-utility:generate-unique-token))
      (setf #2# (hash-password password)))))

(defmethod print-object ((application-user application-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S" user-id user-login))))

(defmethod print-object ((application-user application-meta-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-login create-date disable))))

(defmethod get-user-path ((application-user application-user) (data-store-location jfh-store:data-store-location))
  "Input: application-user and app-configuration. Output: user path."
  (with-accessors ((user-path-root jfh-store:user-path-root)) data-store-location
    (with-accessors ((user-id user-id)) application-user
      (format nil "~A~A/" user-path-root user-id))))

(defmethod save-user (file-name user-info-list (application-user application-user) (data-store-location jfh-store:data-store-location))
  "Input: file-name, user info list (not a class), application-user and data-store-location. Output: user info list. Persist application user info."
  (let ((user-info-file-path (format nil "~A~A" (get-user-path application-user data-store-location) file-name)))
    (jfh-store:write-complete-file user-info-file-path user-info-list)))

(defmethod save-application-user ((application-user application-meta-user) (data-store-location jfh-store:data-store-location))
  "Input: application-meta-user and data-store-location. Output: serialized application-meta-user. Persist application user info."
  (let ((file-name "user.sexp")
        (user-info-list (list
                         :user-id (user-id application-user)
                         :user-login (user-login application-user)
                         :create-date (create-date application-user)
                         :disable (disable application-user))))
    (save-user file-name user-info-list application-user data-store-location)
    (when (next-method-p)
      (call-next-method))))

(defmethod save-application-user ((application-user application-secure-user) (data-store-location jfh-store:data-store-location))
  "Input: application-secure-user and data-store-location. Output: serialized application-user. Persist application user info."
  (let ((file-name "hash.sexp")
        (user-info-list (list
                         :user-password (user-password application-user))))
    (save-user file-name user-info-list application-user data-store-location)))

(defmethod print-object ((user-index-entry user-index-entry) stream)
  "Print user index entry."
  (print-unreadable-object (user-index-entry stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login)) user-index-entry
      (format stream
	      "User ID: ~A, User Name: ~S" user-id user-login))))

(defmethod make-user-index-entry ((application-user application-user))
  "Input: application-user. Output: user index entry."
  (make-instance 'user-index-entry
		 :user-login (user-login application-user)
		 :user-id (user-id application-user)))

(defmethod user-index-entry->list ((user-index-entry user-index-entry))
  "Input: user index entry. Output: regular list. Conversion function."
  (list
   :user-id (user-id user-index-entry)
   :user-login (user-login user-index-entry)))

(defmethod save-new-application-user ((application-user application-meta-user) (data-store-location jfh-store:data-store-location))
  "Input: application-meta-user and data-store-location. Output: application-user. Persist application user info."
  (let* ((user-path-root (user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root)))
    (flet ((callback (user-index)
             (push (user-index-entry->list (make-user-index-entry application-user)) user-index)
             (jfh-store:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-store:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (find-user-path application-user data-store-location))
      (save-application-user application-user data-store-location))))

(defmethod find-user-index-entry (user-login (data-store-location jfh-store:data-store-location))
  "Input: User ID and app-configuration. Output: user index entry."
  (let* ((user-path-root (user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path)))
    (find-if (lambda (entry) (string= (getf entry :user-login) user-login)) user-index)))
