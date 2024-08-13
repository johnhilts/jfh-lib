(in-package #:jfh-remoting)

(defclass remoting-configuration ()
  ((%swank-port
    :reader swank-port
    :initarg :swank-port)
   (%swank-interface
    :reader swank-interface
    :initarg :swank-interface))
  (:documentation "Remoting configurations."))

(defgeneric make-remoting-configuration (data-store-location)
  (:documentation "Input: data store location. Output: remoting configuration."))

(defgeneric save-remoting-configuration (remoting-configuration data-store-location)
  (:documentation "Input: remoting-configuration and data store location. Output: remoting configuration serialized into a plist."))

(defgeneric start-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Start swank with the provided configuration settings."))
 
(defgeneric stop-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Stop swank with the provided configuration settings."))
