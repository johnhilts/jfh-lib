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

(defgeneric get-data-store-location (data-store-location)
  (:documentation "Input: data store location, or application root path. *Not* user specific, but the argument can have user-specific information."))

(defgeneric get-user-data-store-location (id data-store-location)
  (:documentation "Input: ID (such as a User ID) and data store location. Output: path."))

(defgeneric save-index (data index-name)
  (:documentation "Input: data and index-name. Index-name shouldn't assume the type of persistence, that will be filled in with the method. Output: defined by method."))

(defgeneric save-data (data name)
  (:documentation "Input: data and name; name is like a label to associate with the data. Name shouldn't assume the type of persistence, that will be filled in with the method. Output: defined by method."))
