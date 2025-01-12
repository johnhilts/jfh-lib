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

(defmethod get-user-info ((user-id application-user-id))
  "Search for user info in file system."
  (user-entry->application-user (read-user-info (jfh-user:user-id user-id) "user.sexp")))

(defmethod get-user-info ((user-login application-user-login))
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-login jfh-store:*data-store-location*))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-user-info ((user-fingerprint application-user-fingerprint))
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-fingerprint jfh-store:*data-store-location*))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-user-info ((user-login string)) ;; TODO - can we remove this?
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-login jfh-store:*data-store-location*))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-user-info ((user-fingerprint simple-vector)) ;; TODO - can we remove this?
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-fingerprint jfh-store:*data-store-location*))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-secure-user-info ((user-login application-user-login))
  "Search for secure user info in file system."
  (let* ((application-user (get-user-info (user-login user-login))))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defmethod get-secure-user-info ((user-fingerprint application-user-fingerprint))
  "Search for secure user info in file system."
  (let* ((application-user (get-user-info (user-fingerprint user-fingerprint))))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defmethod get-secure-user-info ((user-login string)) ;; TODO - can we remove this?
  "Search for secure user info in file system."
  (let* ((application-user (get-user-info user-login)))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defmethod get-secure-user-info ((user-fingerprint simple-vector)) ;; TODO - can we remove this?
  "Search for secure user info in file system."
  (let* ((application-user (get-user-info user-fingerprint)))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defmethod save-user (file-name user-info-list (application-user application-user) (data-store-location jfh-store:data-store-location)) ;; TODO remove data-store-location from lambda list
  "Input: file-name, user info list (not a class), application-user and data-store-location. Output: user info list. Persist application user info."
  (let ((user-info-file-path (format nil "~A~A" (get-user-path application-user data-store-location) file-name))) ;; TODO path etc will be figured out in jfh-store; only pass the LABEL
    (jfh-store:write-complete-file user-info-file-path user-info-list))) ;; replace with jfh-store:save-object

(defmethod save-application-user ((application-user application-meta-user) (data-store-location jfh-store:data-store-location))
  "Input: application-meta-user and data-store-location. Output: serialized application-meta-user. Persist application user info."
  (let ((file-name "user.sexp") ;; TODO rename to "label"; remove the ".sexp"
        (user-info-list (list ;; TODO this should be moved to a mapping file so it's not mixed in with other code
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
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root)))
    (flet ((callback (user-index)
             (push (user-index-entry->list (make-user-index-entry application-user)) user-index)
             (jfh-store:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-store:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (get-user-path application-user data-store-location))
      (save-application-user application-user data-store-location))))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-id application-user-id) (data-store-location jfh-store:data-store-location)) ;; TODO probably don't need this - if we already have the user ID why would we need to bother with the index?
  "Input: User ID and app-configuration. Output: user index entry."
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path))) ;; note: this is where the error is signalled if the user index file is missing
    (find-if (lambda (entry) (string= (getf entry :user-id) (user-id user-id))) user-index)))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-login application-user-login) (data-store-location jfh-store:data-store-location))
  "Input: User Login and app-configuration. Output: user index entry."
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path))) ;; note: this is where the error is signalled if the user index file is missing
    (find-if (lambda (entry) (string= (getf entry :user-login) (user-login user-login))) user-index)))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-fingerprint application-user-fingerprint) (data-store-location jfh-store:data-store-location))
  "Input: User fingerprint and app-configuration. Output: user index entry."
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-fingerprint-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path))) ;; note: this is where the error is signalled if the user index file is missing

    (find-if (lambda (entry) (equalp (getf entry :user-fingerprint) (user-fingerprint user-fingerprint))) user-index)))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-login string) (data-store-location jfh-store:data-store-location)) ;; TODO - can we remove this?
  "Input: User ID and app-configuration. Output: user index entry."
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path))) ;; note: this is where the error is signalled if the user index file is missing
    (find-if (lambda (entry) (string= (getf entry :user-login) user-login)) user-index)))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-fingerprint simple-vector) (data-store-location jfh-store:data-store-location)) ;; TODO - can we remove this?
  "Input: User fingerprint and app-configuration. Output: user index entry."
  (let* ((user-path-root (jfh-store:user-path-root data-store-location))
         (user-index-file-path (get-user-fingerprint-index-file-path user-path-root))
	 (user-index (jfh-store:fetch-or-create-data user-index-file-path))) ;; note: this is where the error is signalled if the user index file is missing

    (find-if (lambda (entry) (equalp (getf entry :user-fingerprint) user-fingerprint)) user-index)))
