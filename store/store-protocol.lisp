(in-package #:jfh-store)

(defclass data-store-location ()
  ((%settings-file-path
    :reader settings-file-path
    :initarg :settings-file-path
    :documentation "Example: \"./configuration.sexp\"")
   (%user-path-root
    :reader user-path-root
    :initarg :user-path-root
    :initform (error "Value required for :user-path-root")
    :documentation "Example: \"./users/\"")))

(defclass file-store-location ()
  ((%location
    :reader location
    :initarg :location
    :initform "."
    :documentation "Flat file root directory.")))

(defclass database-store-location ()
  ((%host
    :reader host
    :initarg :host
    :initform "."
    :documentation "Database server.")
   (%location
    :reader location
    :initform "DB cataglog name"
    :documentation "DB catalog.")))

(defclass user-data-store-location (file-store-location)
  ((%location
    :initarg :location
    :initform "./users"))) ;; put this in a dynamic variable?

(defclass data-store ()
  ((%label
    :reader label
    :initarg :label
    :documentation "DB table name, file name")
   (%key
    :reader key
    :initarg :key
    :documentation "field(s) used for indexing")))

(defclass store-database ()
  ((%data
    :reader data
    :initarg :data
    :documentation "Data to persist.")))

(defclass store-file ()
  ((%data
    :reader data
    :initarg :data
    :documentation "Data to persist.")
   (%label
    :reader label
    :initarg :label
    :documentation "Place to store data.")
   (%key
    :reader key
    :initarg :key
    :documentation "Identifier for data.")))

(defclass store-data (store-file)
  ())

(defclass user-store-data (store-data)
  ())

(defclass user-index-store-data (store-data)
  ())

;; TODO you can't name a class with "object"
(defclass store-object (file-store-location data-store)
  ()
  (:documentation "fields set by app"))

(defclass user-store-object (user-data-store-location store-object)
  ()
  (:documentation "fields set by app"))

(defclass user-index-store-object (user-store-object)
  ()
  (:documentation "fields set by app"))

(defgeneric get-data-store-location (data-store-location)
  (:documentation "Input: data store location, or application root path. *Not* user specific, but the argument can have user-specific information."))

(defgeneric get-user-data-store-location (id data-store-location)
  (:documentation "Input: ID (such as a User ID) and data store location. Output: path."))

(defgeneric save-index (data index-name)
  (:documentation "Input: data and index-name. Index-name shouldn't assume the type of persistence, that will be filled in with the method. Output: defined by method."))

(defgeneric save-data (data name key)
  (:documentation "Input: data and name key; name is like a label to associate with the data. Name shouldn't assume the type of persistence, that will be filled in with the method. The key is like a unique ID to help keep data separated. Output: defined by method."))

(defgeneric get-data (store-object))

(defgeneric save-user-data (store-data))

(defgeneric internal/get-data-by-location (location store-object))

(defgeneric internal/save-data-by-location (store data))

(defgeneric serialize-object->list (object accessors)
  (:documentation "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used for data with 1 row."))
