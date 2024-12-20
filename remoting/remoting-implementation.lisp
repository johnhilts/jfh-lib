(in-package #:jfh-remoting)

(defparameter *remoting-configuration* nil)

(defmethod print-object ((remoting-configuration remoting-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (remoting-configuration stream :type t)
    (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) remoting-configuration
      (format stream
	      "Swank Port: ~D, Swank Interface: ~A"
	      swank-port swank-interface))))

(defmethod print-object ((remoting-configuration actual-remoting-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (remoting-configuration stream :type t)
    (with-accessors ((swank-port swank-port) (swank-interface swank-interface) (actual-swank-port actual-swank-port)) remoting-configuration
      (format stream "Actual Swank Port: ~D, " actual-swank-port)
      (call-next-method))))

(defmethod make-remoting-configuration ((data-store-location jfh-store:data-store-location))
  "Get configuration info from the file system and hydrate remoting-configuration object.
Input: default configuration values.
Output: remoting-configuration object."
  (with-accessors ((settings-file-path jfh-store:settings-file-path)) data-store-location
    (jfh-store:make-instance-from-data-store
     'remoting-configuration
     (list :swank-port '? :swank-interface '?)
     nil nil
     (lambda (_ __) (declare (ignore _ __)) "./"))))

(defmethod make-actual-remoting-configuration-OLD ((remoting-configuration remoting-configuration) actual-swank-port)
  "Create actual remoting configuration based on the default settings. Meant to be used by START-SWANK.
Input: default configuration object.
Output: actual-remoting-configuration object."
  (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) remoting-configuration
    (make-instance 'actual-remoting-configuration :swank-port swank-port :swank-interface swank-interface :actual-swank-port actual-swank-port)))

;; TODO - do we need this??
(defmethod save-remoting-configuration ((remoting-configuration remoting-configuration) (data-store-location jfh-store:data-store-location))
  "Input: remoting-configuration and data store location. Output: remoting configuration serialized into a plist."
  (with-accessors ((settings-file-path jfh-store:settings-file-path)) data-store-location
    (jfh-store:write-instance-to-data-store
     'remoting-configuration
     remoting-configuration
     (list 'swank-port 'swank-interface)
     nil nil
     (lambda (_ __) (declare (ignore _ __)) "./"))))

(defmethod start-swank ((remoting-configuration remoting-configuration))
  "Input: remoting-configuration. Start swank on the configured port."
  (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) remoting-configuration
    (flet ((start-swank-server ()
             (restart-case (swank:create-server :port swank-port
					        :interface swank-interface
					        :dont-close t)
	       (skip-swank-start ()
                 :report "Skip Swank Start."
                 (format t "Swank start skipped.")
                 (throw 'swank-start swank-port)))))
      (let ((*debug-io* (make-broadcast-stream)))
        (catch 'swank-start
          (let ((actual-port (start-swank-server)))
	    (format t "Started swank at port: ~A." actual-port)
            (jfh-configuration:bind-configuration remoting-configuration actual-port)))))))

(defmethod stop-swank ((remoting-configuration actual-remoting-configuration))
  (with-accessors ((actual-swank-port actual-swank-port)) remoting-configuration
    (when actual-swank-port
      (swank:stop-server actual-swank-port)
      (format t "Stopped swank at port: ~A." actual-swank-port))))

(defmethod jfh-configuration:bind-configuration ((type (eql 'remoting)) &optional (data-store-location jfh-store:*data-store-location*))
  "Input: the type, remoting. Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."
  (let ((remoting-configuration (make-remoting-configuration data-store-location)))
    (setf *remoting-configuration* remoting-configuration)
    remoting-configuration))

(defmethod jfh-configuration:bind-configuration ((default-remoting-configuration remoting-configuration) &optional (actual-swank-port integer))
  "Input: the default remoting configuration. Output: the actual remoting configuration object. Configuration objects are NOT in an inheritance hierarchy."
  (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) default-remoting-configuration
    (let ((actual-remoting-configuration
            (make-instance 'actual-remoting-configuration :swank-port swank-port :swank-interface swank-interface :actual-swank-port actual-swank-port)))
      (setf *remoting-configuration* actual-remoting-configuration)
      actual-remoting-configuration)))

(defmethod jfh-configuration:get-configuration ((type (eql 'remoting)))
  "Input: type such as 'app, 'remoting, or 'web. Output: configuration object. Configuration objects are NOT in an inheritance hierarchy."
  *remoting-configuration*)
