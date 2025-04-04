(in-package #:jfh-store)

;; TODO - come back and add documentation; we can grab from the files in the "old" directory

(defvar *app-data-path* ".")

(defclass database () ()) ;; unused ATM
(defclass flat-file () ())

(defclass user-index (flat-file)
  ((%user-id :reader user-id :initarg :user-id))) ;; index file in the users folder
(defclass user-settings (flat-file)
  ((%user-id :reader user-id :initarg :user-id))) ;; user settings in users/{user-id}/ sub-folder
(defclass user-data (flat-file)
  ((%data-id :reader data-id :initarg :data-id)
   (%user-id :reader user-id :initarg :user-id))) ;; data in users/{user-id}/ sub-folder
(defclass user-data-large (data) ()) ;; unused for now, but meant to handle large datasets that are difficult to just READ
(defclass config-data (flat-file) ()) ;; configuration data in app root folder

;; TODO - should these be defined here? Do they have to be ??
(defclass user-login-index (user-index)
  ((%user-login :reader user-login :initarg :user-login)))
(defclass user-fingerprint-index (user-index)
  ((%user-fingerprint :reader user-fingerprint :initarg :user-fingerprint)))
(defclass user-apikey-index (user-index)
  ((%user-apikey :reader user-apikey :initarg :user-apikey)))

;; index support
(defparameter *indexed-user-fields* '(:user-id user-index :user-login user-login-index :user-fingerprint user-fingerprint-index :user-apikey user-apikey-index))

(defgeneric make-instance* (class-name &key where user-id))
(defgeneric serialize-object->list (object accessors))
(defgeneric save-object (object &key readers save-name))
(defgeneric save-index (index &key readers save-name))
(defgeneric update-data (object readers save-name))
