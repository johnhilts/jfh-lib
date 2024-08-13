(in-package #:jfh-configuration)

(defmethod print-object ((application-configuration application-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (application-configuration stream :type t)
    (with-accessors ((swank-port swank-port)
                     (swank-interface swank-interface)
                     (settings-file-path settings-file-path)
                     (user-path-root user-path-root))
        application-configuration
      (format stream
	      "~:[~:;Swank Port: ~:*~d~]~:[~:;, Swank Interface: ~:*~a~]~:*~:[~:;, ~]Settings File: ~s, User Path: ~s"
	      swank-port swank-interface settings-file-path user-path-root))))

(defmethod make-application-configuration ((data-store-location jfh-store:data-store-location))
  "Get configuration info from the file system and hydrate application-configuration object.
Input: default configuration values.
Output: application-configuration object."
  (with-accessors ((settings-file-path jfh-store:settings-file-path)) data-store-location
    (jfh-store:make-instance-from-data-store
     'application-configuration
     (list :swank-port '? :swank-interface '? :settings-file-path settings-file-path :user-path-root '?)
     nil nil
     (lambda (_ __) (declare (ignore _ __)) "./"))))
