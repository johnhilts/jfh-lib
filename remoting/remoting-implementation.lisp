(in-package #:jfh-remoting)

(defparameter *remoting-configuration* nil)

(defmethod print-object ((remoting-configuration remoting-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (remoting-configuration stream :type t)
    (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) remoting-configuration
      (format stream
	      "Swank Port: ~D, Swank Interface: ~A"
	      swank-port swank-interface))))

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
            actual-port))))))

(defmethod stop-swank ((remoting-configuration remoting-configuration))
  (with-accessors ((swank-port swank-port)) remoting-configuration
    (when swank-port
      (swank:stop-server swank-port)
      (format t "Stopped swank at port: ~A." swank-port))))