(in-package #:jfh-configuration)

(defclass application-configuration ()
  ((%swank-port
    :reader swank-port
    :initarg :swank-port)
   (%swank-interface
    :reader swank-interface
    :initarg :swank-interface)
   (%settings-file-path
    :reader settings-file-path
    :initarg :settings-file-path)
   (%user-path-root
    :reader user-path-root
    :initarg :user-path-root
    :initform (error "Value required for :user-path-root")))
  (:documentation "Application configurations."))

(defgeneric make-application-configuration (data-store-location)
  (:documentation "Input: data store location. Output: application configuration."))
