(in-package #:jfh-remoting)

(defclass remoting-configuration (jfh-store:config-data)
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

(defgeneric make-actual-remoting-configuration-OLD (data-store-location actual-swank-port)
  (:documentation "Input: default configuration settings. Output: actual remoting configuration."))

(defgeneric start-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Start swank with the provided configuration settings."))
 
(defgeneric stop-swank (remoting-configuration)
  (:documentation "Input: remoting-configuration. Stop swank with the provided configuration settings."))
