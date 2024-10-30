(in-package #:jfh-configuration)

(defparameter *application-configuration* nil)

(defmethod print-object ((application-configuration application-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (application-configuration stream :type t)
    (with-accessors ((settings-file-path settings-file-path)
                     (user-path-root user-path-root))
        application-configuration
      (format stream
	      "Settings File: ~s, User Path: ~s"
	       settings-file-path user-path-root))))

(defmethod make-application-configuration ((application-root-path string))
  "Get configuration info from the file system and hydrate application-configuration object.
Input: application root path.
Output: application-configuration object."
  (jfh-store:make-instance-from-data-store
   'application-configuration
   (list :settings-file-path application-root-path :user-path-root '?)
   nil nil
   (lambda (_ __) (declare (ignore _ __)) application-root-path)))

(defmethod bind-configuration ((type (eql 'application)) &optional application-root-path)
  "Input: the application root path. Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."
  (let ((configuration (make-application-configuration application-root-path)))
    (setf *application-configuration* configuration)
    configuration))

(defmethod get-configuration ((type (eql 'app)))
  "Input: type such as 'app, 'remoting, or 'web. Output: configuration object. Configuration objects are NOT in an inheritance hierarchy."
  *application-configuration*)
