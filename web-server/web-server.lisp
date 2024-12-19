(in-package #:jfh-web-server)

(defvar *web-application*)

(defvar *web-configuration*)

(defparameter *client-cert-missing* nil)

(defparameter *static-path-maps* ())

;; TODO: should this part go into "internal"? #-start-#

(defun my-log (string)
  (with-open-file (out "./log.txt" :direction :output :if-does-not-exist :create :if-exists :append)
    (prin1 string out)))

(defun get-my-cert-path (&optional (default-path "./certs/set9/"))
  (restart-case (if (probe-file default-path)
                    default-path
                    (error 'cert-file-missing :pathname default-path "Couldn't find certificate path ~S:" default-path))
    (use-value (value)
      :report
      (lambda (s) ;Argument s is a stream
        (format s "Specify a value of ~S to use this time." default-path))
      :interactive
      (lambda ()
        (format *query-io* "Enter a different path - Example: ~A: " default-path)
        (finish-output *query-io*)      ; necessary for tunnels
        (ignore-errors (list (read-line *query-io*))))
      value)))

;; TODO: should this part go into "internal"? #-end-#
 
(defun make-web-configuration-OLD (&optional (ssl-port nil) (http-port 8080) (static-root "")) ;; TODO - remove
  "Get configuration info from the file system and hydrate web-configuration object.
Input: default configuration values.
Output: web-configuration object."
  (let* ((data-store-location jfh-store:*data-store-location*)
         (call-back #'(lambda (settings)
			(if settings
			    (make-instance 'web-configuration
					   :ssl-port (getf settings :ssl-port)
					   :http-port (getf settings :http-port)
					   :static-root (getf settings :static-root))
	                    (make-instance 'web-configuration
					   :ssl-port ssl-port
					   :http-port http-port
					   :static-root static-root)))))
    (jfh-store:fetch-or-create-data (jfh-store:settings-file-path data-store-location) call-back)))

(defun %make-web-application-core (hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration)
  "Constructor for web-application"
  (make-instance 'web-application
		 :hunchentoot-acceptor hunchentoot-acceptor
		 :hunchentoot-ssl-acceptor hunchentoot-ssl-acceptor
		 :web-configuration web-configuration))


(defun add-static-content-handlers ()
  "Add handlers for provided static content web/path mappings."
  (mapc
   (lambda (mapping)
     (push
      (tbnl:create-static-file-dispatcher-and-handler (car mapping) (cdr mapping))
      tbnl:*dispatch-table*))
   *static-path-maps*))

(defun add-static-path-map (web-path physical-path)
  "add mapping pairs to be used to expose static assets from the web server."
  (pushnew
   (cons web-path physical-path)
   *static-path-maps*
   :key #'car
   :test #'string=))
