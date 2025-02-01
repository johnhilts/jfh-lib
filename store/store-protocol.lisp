(in-package #:jfh-store)

;; TODO - come back and add documentation; we can grab from the files in the "old" directory

(defvar *app-data-path* ".")

(defclass database-store ()
  ((%host
    :reader host)
   (%catalog
    :reader catalog)
   (%label
    :reader label))) ;; unused ATM

(defclass file-store ()
  ((%path
    :reader path
    :initarg :path
    :initform *app-data-path*)
   (%label
    :reader label
    :initarg :label)))

(defclass user-store (file-store)
  ((%path
    :reader path
    :initform (format nil "~A/users" *app-data-path*))))

(defclass user-data-store (user-store)
  ((%key
    :reader key
    :initarg :key)))

(defclass user-index-store (user-store) ())

(defclass user-config-store (user-data-store) ())

(defclass data ()
  ((%serialized-data
    :reader serialized-data
    :initarg :serialized-data)))

(defclass config-data (data) ())

(defclass user-config-data (data) ())

(defclass user-index-data (data) ())

(defclass user-data (data) ())

(defclass user-data-large (data) ()) ;; unused for now, but meant to handle large datasets that are difficult to just READ

(defgeneric get-data-path (file-store))

(defgeneric get-data (file-store))

(defgeneric serialize-object->list (object accessors)
  (:documentation "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used for data with 1 row."))

(defgeneric save-data (file-store data))
