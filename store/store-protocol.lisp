(in-package #:jfh-store)

;; TODO - come back and add documentation; we can grab from the files in the "old" directory

(defvar *app-data-path* ".")

(defclass database () ()) ;; unused ATM
(defclass flat-file () ())

(defclass reader-entry ()
  ((%reader-name :reader reader-name :initarg :reader-name)
   (%slot-boundp-check :reader slot-boundp-check :initarg :slot-boundp-check)))

(defclass user-index (flat-file)
  ((%user-id :reader user-id :initarg :user-id))) ;; index file in the users folder
(defclass user-settings (flat-file)
  ((%user-id :reader user-id :initarg :user-id))) ;; user settings in users/{user-id}/ sub-folder
(defclass user-data (flat-file)
  ((%data-id :reader data-id :initarg :data-id)
   (%user-id :reader user-id :initarg :user-id))) ;; data in users/{user-id}/ sub-folder
(defclass user-data-large (user-data) ()) ;; unused for now, but meant to handle large datasets that are difficult to just READ
(defclass config-data (flat-file) ()) ;; configuration data in app root folder

;; TODO - should these be defined here? Do they have to be ??
(defclass user-login-index (user-index)
  ((%user-login :reader user-login :initarg :user-login)))
(defclass user-fingerprint-index (user-index)
  ((%user-fingerprint :reader user-fingerprint :initarg :user-fingerprint)))
(defclass user-apikey-index (user-index)
  ((%user-apikey :reader user-apikey :initarg :user-apikey)))

(defvar *non-serialized-fields* ())

(defgeneric make-instance* (class-name &key where user-id))
(defgeneric serialize-object->list (object))
(defgeneric save-object (object &key save-name))
(defgeneric delete-object (object &key save-name))
(defgeneric save-index (index &key save-name))
(defgeneric update-data (object save-name))
(defgeneric delete-data (object save-name))
(defgeneric get-direct-readers (class))

