(in-package #:jfh-configuration)

(defclass application-configuration ()
  ((%settings-file-path
    :reader settings-file-path
    :initarg :settings-file-path)
   (%user-path-root
    :reader user-path-root
    :initarg :user-path-root
    :initform (error "Value required for :user-path-root")))
  (:documentation "Application configurations."))

(defgeneric make-application-configuration (data-store-location)
  (:documentation "Input: data store location. Output: application configuration."))

(defgeneric bind-configuration (type)
  (:documentation "Input: a type such as 'remoting or 'web, OR a string for the default root path.
Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."))

(defgeneric rebind-configuration (default-settings configuration-value)
  (:documentation "Input: a configuration object pre-loaded with default values, and a configuration value.
Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."))

(defgeneric get-configuration (type)
  (:documentation "Input: type such as 'app, 'remoting, or 'web. Output: configuration object. Configuration objects are NOT in an inheritance hierarchy."))
