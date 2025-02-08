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

(defmethod print-object ((application-user application-secure-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-password user-password)) application-user
      (format stream
	      "User Password: ****"))))

(defmethod print-object ((application-user application-meta-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-login create-date disable))))

(defmethod get-user-path ((application-user application-user))
  "Input: application-user and app-configuration. Output: user path."
  "can we stop using this?"
;; (with-accessors ((user-path-root jfh-store:user-path-root)) jfh-store:*data-store-location*
  ;;   (with-accessors ((user-id user-id)) application-user
  ;;     (format nil "~A~A/" user-path-root user-id)))
  )

(defmethod get-user-info ((user-id application-user-id))
  "Search for user info in file system."
  (jfh-store:make-instance* 'application-user :key (user-id user-id))
  ;; (user-entry->application-user (read-user-info (jfh-user:user-id user-id) "user.sexp"))
  )

(defmethod get-user-info ((user-login application-user-login))
  "Search for user info in file system."
  (jfh-store:make-instance* 'jfh-user:application-user :key (user-id user-login)) ;; TODO - this won't work because application-user-login doesn't have a user-id accessor
;; (let* ((user-index-entry (get-user-index-entry user-login))
  ;;        (user-id (getf user-index-entry :user-id)))
  ;;   (when user-id
  ;;     (user-entry->application-user (read-user-info user-id "user.sexp"))))
  )

(defmethod get-user-info ((user-fingerprint application-user-fingerprint))
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-fingerprint))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-user-info ((user-login string)) ;; TODO - can we remove this?
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-login))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defmethod get-user-info ((user-fingerprint simple-vector)) ;; TODO - can we remove this?
  "Search for user info in file system."
  (let* ((user-index-entry (get-user-index-entry user-fingerprint))
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

(defmethod save-application-user ((application-user application-meta-user))
  "Input: application-meta-user and data-store-location. Output: serialized application-meta-user. Persist application user info."
  (jfh-store:save-object application-user :readers '(user-id user-login create-date disable) :key (user-id application-user) :name "application-meta-user")
  (when (next-method-p)
    (call-next-method)))

(defmethod save-application-user ((application-user application-secure-user))
  "Input: application-secure-user and data-store-location. Output: serialized application-user. Persist application user info."
  (jfh-store:save-object application-user :readers '(user-password user-fingerprint user-api-key) :key (user-id application-user) :name "application-secure-user"))

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

(defmethod user-index-entry->list ((user-index-entry user-index-entry)) ;; TODO will need to be able to handle multiple kinds of indexes
  "Input: user index entry. Output: regular list. Conversion function."
  (list
   :user-id (user-id user-index-entry)
   :user-login (user-login user-index-entry)))

(defun get-user-identifier-class (application-user)
  "Input: a user object. Output: the class name relevant to which index user file should be used."
  (typecase application-user
    (application-user-fingerprint 'application-user-fingerprint) ;; TODO add more types as needed
    (otherwise nil)))

(defmethod save-new-application-user ((application-user application-meta-user))
  "Input: application-meta-user and data-store-location. Output: application-user. Persist application user info."
  (let* ((index-entry (make-user-index-entry application-user)))
    (jfh-store:save-object index-entry (list 'user-id 'user-login)))
  (save-application-user application-user))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-login application-user-login))
  "Input: User Login and app-configuration. Output: user index entry."
  (jfh-store:make-instance* 'user-login-index-entry :key (user-login user-login) :field :user-login))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-fingerprint application-user-fingerprint))
  "Input: User fingerprint and app-configuration. Output: user index entry."
  (jfh-store:make-instance* 'jfh-user::user-fingerprint-index-entry :key (user-fingerprint user-fingerprint) :field :user-fingerprint))
