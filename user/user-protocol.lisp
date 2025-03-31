(in-package #:jfh-user)

(defclass application-user (jfh-store:user-settings)
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
    :initform "")
   (%user-fingerprint
    :reader user-fingerprint
    :initarg :user-fingerprint
    :initform #())
   (%user-api-key
    :reader user-api-key
    :initarg :user-api-key
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

(defclass application-user-identifier (jfh-store:user-index)
  ()
  (:documentation "User identifier. Base class for the different kinds of user identifiers. This is based on the assumption that user information is keyed to from 1 piece of input."))

(defclass application-user-login (application-user-identifier)
  ((%user-login
    :reader user-login
    :initarg :user-login))
  (:documentation "User login. Meant for what a user interactively inputs as their User Login."))

(defclass application-user-id (application-user-identifier)
  ()
  (:documentation "User ID. Meant for an **internal** user ID that a user typically would not see in a UI."))

(defclass application-user-api-key (application-user-identifier)
  ((%user-api-key
    :reader user-api-key
    :initarg :user-api-key))
  (:documentation "User API key. Meant for an API key generated by the application that a client would then use in an HTTP request to identify itself, or its user."))

(defclass application-user-fingerprint (application-user-identifier)
  ((%user-fingerprint
    :reader user-fingerprint
    :initarg :user-fingerprint
    ;; :type simple-vector
    ))
  (:documentation "User fingerprint. Meant as the fingerprint / thumbprint of an SSL certificate generated by the application that a client would then use in an HTTP request to identify itself, or its user."))

(defclass user-login-index-entry (application-user-login)
  ()
  (:documentation "User index entry. Link User ID to persisted data."))

(defclass user-fingerprint-index-entry (application-user-fingerprint)
  ()
  (:documentation "User index entry. Link User ID to persisted data."))

(defgeneric get-user-index-entry (user-login)
  (:documentation "Input: User name (login). Output: user index entry."))

(defgeneric get-user-info (user-identifier)
  (:documentation "Input: something that uniquely identifies a user. Output: application-meta-user"))

(defgeneric get-secure-user-info (user-identifier)
  (:documentation "Input: something that uniquely identifies a user. Output: application-secure-user."))

(defgeneric make-user-index-entry (application-user)
  (:documentation "Input: application-user. Output: user index entry."))

(defgeneric save-user (file-name user-info-list application-user) ;; TODO think of better function/method names
  (:documentation "Input: file-name, user info list (not a class), application-user and data-store-location. Output: user info list. Persist application user info."))

(defgeneric save-application-user (application-user)
  (:documentation "Input: application-user. Output: application-user. Persist application user info."))

(defgeneric save-new-application-user (application-user)
  (:documentation "Input: application-user. Output: application-user. Persist *NEW* application user info."))
