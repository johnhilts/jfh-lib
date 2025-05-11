(in-package #:jfh-remoting)

(defparameter *remoting-configuration* nil)

(defun make-remoting-configuration ()
  "Get configuration info from the file system and hydrate remoting-configuration object.
Output: remoting-configuration object."
  (jfh-store:make-instance* 'remoting-configuration))
