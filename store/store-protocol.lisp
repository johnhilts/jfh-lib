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
  (:documentation "Input: data store location. *Not* user specific, but the argument can have user-specific information."))

(defgeneric get-user-data-store-location (id data-store-location)
  (:documentation "Input: ID (such as a User ID) and data store location. Output: path."))
