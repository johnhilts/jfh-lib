(in-package #:jfh-remoting)

(defclass remoting-configuration ()
  ((%swank-port
    :reader swank-port
    :initarg :swank-port)
   (%swank-interface
    :reader swank-interface
    :initarg :swank-interface))
  (:documentation "Remoting configurations."))

(defclass actual-remoting-configuration (remoting-configuration)
  ((%actual-swank-port
    :reader actual-swank-port
    :initarg :actual-swank-port))
  (:documentation "The remoting configuration actually used by the app. Can differ from configured default settings."))

(defgeneric make-remoting-configuration (data-store-location)
  (:documentation "Input: data store location. Output: remoting configuration."))

(defgeneric make-actual-remoting-configuration-OLD (data-store-location actual-swank-port)
  (:documentation "Input: default configuration settings. Output: actual remoting configuration."))

;; TODO - do we need this??
(defgeneric save-remoting-configuration (remoting-configuration data-store-location)
  (:documentation "Input: remoting-configuration and data store location. Output: remoting configuration serialized into a plist."))

(defgeneric start-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Start swank with the provided configuration settings."))
 
(defgeneric stop-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Stop swank with the provided configuration settings."))
