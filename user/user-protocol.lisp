(in-package #:jfh-user)

(defclass application-user ()
  ((%user-id
    :reader user-id
    :initarg :user-id
    :initform "")
   (%user-login
    :reader user-login
    :initarg :user-login))
  (:documentation "Application user info - the very bare minimum."))

(defclass application-secure-user (application-user)
  ((%user-password
    :reader user-password
    :initarg :user-password
    :initform ""))
  (:documentation "Application user secure info."))

(defclass application-meta-user (application-user)
  ((%create-date
    :reader create-date
    :initarg :create-date
    :initform (get-universal-time))
   (%disable
    :reader disable
    :initarg :disable
    :initform nil))
  (:documentation "Application user info with meta-data."))

(defclass user-index-entry ()
  ((%user-login
    :reader user-login
    :initarg :user-login)
   (%user-id
    :reader user-id
    :initarg :user-id))
  (:documentation "User index entry. Link User ID to persisted data."))

(defgeneric get-user-path (application-user data-store-location)
  (:documentation "Input: application-user and data-store-location. Output: user path."))

(defgeneric get-user-index-entry (user-login data-store-location)
  (:documentation "Input: User name (login) and data-store-location. Output: user index entry."))

(defgeneric make-user-index-entry (application-user)
  (:documentation "Input: application-user. Output: user index entry."))

(defgeneric user-index-entry->list (user-index-entry)
  (:documentation "Input: user index entry. Output: regular list. Conversion function."))

(defgeneric save-user (file-name user-info-list application-user data-store-location) ;; TODO think of better function/method names
  (:documentation "Input: file-name, user info list (not a class), application-user and data-store-location. Output: user info list. Persist application user info."))

(defgeneric save-application-user (application-user data-store-location)
  (:documentation "Input: application-user and data-store-location. Output: application-user. Persist application user info."))

(defgeneric save-new-application-user (application-user data-store-location)
  (:documentation "Input: application-user. Output: application-user. Persist *NEW* application user info."))

(defgeneric hash-user-password (application-secure-user)
  (:documentation "Input: application-secure-user. Output: Encrypted text."))
