(in-package #:jfh-user)

(defmethod initialize-instance :after ((application-secure-user application-secure-user) &key)
  "Initializations:
- Encrypt the user password. This is meant to prevent the plain text password from being in memory.
- Set the User ID to a unique ID."
  (let ((user-id #1=(slot-value application-secure-user 'jfh-store::%user-id)) ;; TODO breaking encapsulation with ::
        (password #2=(slot-value application-secure-user '%user-password))
        (fingerprint #3=(slot-value application-secure-user '%user-fingerprint))
        (api-key #4=(slot-value application-secure-user '%user-api-key)))
    (when (zerop (length user-id))
      (setf #1# (jfh-utility:generate-unique-token))
      (setf #2# (hash-password password))
      (setf #3# (hash-password fingerprint))
      (setf #4# (hash-password api-key)))))

(defmethod print-object ((application-user application-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id jfh-store:user-id) (user-login user-login)) application-user
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
    (with-accessors ((user-id jfh-store:user-id) (user-login user-login) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~S, User Login: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-login create-date disable))))

(defmethod get-user-info ((user-id application-user-id))
  "Search for user info in file system."
  (jfh-store:make-instance* 'application-meta-user :user-id (jfh-store:user-id user-id)))

(defmethod get-user-info ((user-login application-user-login))
  "Search for user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-login-index-entry :where '(:user-login (user-login user-login)))))
    (jfh-store:make-instance* 'application-meta-user :user-id (jfh-store:user-id user-index-entry))))

(defmethod get-user-info ((user-fingerprint application-user-fingerprint))
  "Search for user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-fingerprint-index-entry :where `(:user-fingerprint ,(user-fingerprint user-fingerprint)) )))
    (jfh-store:make-instance* 'application-meta-user :user-id (jfh-store:user-id user-index-entry))))

(defmethod get-secure-user-info ((user-id application-user-id))
  "Search for user info in file system."
  (jfh-store:make-instance* 'application-secure-user :user-id (jfh-store:user-id user-id)))

(defmethod get-secure-user-info ((user-login application-user-login))
  "Search for secure user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-login-index-entry :where '(:user-login (user-login user-login)))))
     (jfh-store:make-instance* 'application-secure-user :user-id (jfh-store:user-id user-index-entry))))

(defmethod get-secure-user-info ((user-fingerprint application-user-fingerprint))
  "Search for secure user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-fingerprint-index-entry :where `(:user-fingerprint ,(hash-password (user-fingerprint user-fingerprint))))))
    (jfh-store:make-instance* 'application-secure-user :user-id (jfh-store:user-id user-index-entry))))

(defmethod save-application-user ((application-user application-meta-user))
  "Input: application-meta-user and data-store-location. Output: serialized application-meta-user. Persist application user info."
  (jfh-store:save-object application-user)
  (when (next-method-p)
    (call-next-method)))

(defmethod save-application-user ((application-user application-secure-user))
  "Input: application-secure-user and data-store-location. Output: serialized application-user. Persist application user info."
  (jfh-store:save-object application-user))

(defmethod print-object ((user-index-entry user-login-index-entry) stream)
  "Print user index entry."
  (print-unreadable-object (user-index-entry stream :type t)
    (with-accessors ((user-id jfh-store:user-id) (user-login user-login)) user-index-entry
      (format stream
	      "User ID: ~A, User Name: ~S" user-id user-login))))

(defmethod make-user-login-index-entry ((application-user application-secure-user))
  "Input: application-secure-user. Output: user login index entry."
  (make-instance 'user-login-index-entry
		 :user-login (user-login application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod make-user-login-index-entry ((application-user application-secure-user))
  "Input: application-secure-user. Output: user login index entry."
  (make-instance 'user-login-index-entry
		 :user-login (user-login application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod make-user-fingerprint-index-entry ((application-user application-secure-user))
  "Input: application-secure-user. Output: user fingerprint index entry."
  (make-instance 'user-fingerprint-index-entry
		 :user-fingerprint (user-fingerprint application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod make-user-apikey-index-entry ((application-user application-secure-user))
  "Input: application-secure-user. Output: user API key index entry."
  (make-instance 'user-api-key-index-entry
		 :user-apikey (user-api-key application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod save-index ((application-user application-secure-user))
  (when (and
         (user-login application-user)
         (plusp (length (user-login application-user))))
    (jfh-store:save-object (make-user-login-index-entry application-user)))
  (when (and
         (user-fingerprint application-user)
         (plusp (length (user-fingerprint application-user))))
    (jfh-store:save-object (make-user-fingerprint-index-entry application-user)))
  (when (and
         (user-api-key application-user)
         (plusp (length (user-api-key application-user))))
    (jfh-store:save-object (make-user-apikey-index-entry application-user))))

(defmethod save-new-application-user ((application-user application-secure-user))
  "Input: application-meta-user and data-store-location. Output: application-user. Persist application user info."
  (save-index application-user)
  (save-application-user application-user))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-login application-user-login))
  "Input: User Login and app-configuration. Output: user index entry."
  (jfh-store:make-instance* 'user-login-index-entry :where '(:user-login (user-login user-login))))

;; TODO add restart so that we have the option to generate the missing user index file
(defmethod get-user-index-entry ((user-fingerprint application-user-fingerprint))
  "Input: User fingerprint and app-configuration. Output: user index entry."
  (jfh-store:make-instance* 'user-fingerprint-index-entry :where '(:user-fingerprint (user-fingerprint user-fingerprint))))
