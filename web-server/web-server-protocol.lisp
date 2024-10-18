(in-package #:jfh-web-server)

(defclass web-configuration ()
  ((%http-port
    :reader http-port
    :initarg :http-port)
   (%ssl-port
    :reader ssl-port
    :initarg :ssl-port)
   (%static-root
    :reader static-root
    :initarg :static-root))
  (:documentation "Web Application configurations."))

(defclass web-application ()
  ((%hunchentoot-acceptor
    :reader hunchentoot-acceptor
    :initarg :hunchentoot-acceptor)
   (%hunchentoot-ssl-acceptor
    :reader hunchentoot-ssl-acceptor
    :initarg :hunchentoot-ssl-acceptor)
   (%web-configuration
    :reader web-configuration
    :initarg :web-configuration))
  (:documentation "Web application."))

(defgeneric make-web-configuration (data-store-location)
  (:documentation "Input: data store location. Output: web configuration."))

(defgeneric start-hunchentoot (web-configuration)
  (:documentation "Input: web-configuration. Start hunchentoot web-server with the provided configuration settings."))

(defgeneric start-web-app (web-configuration)
  (:documentation "Input: web-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."))
;; (documentation 'start-web-app 'function)

(defgeneric stop-hunchentoot (web-application)
  (:documentation "Input: web-application. Stop hunchentoot web-server via the provided web-application object."))

(defgeneric stop-web-app (web-application)
  (:documentation "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released."))
;; (documentation 'stop-web-app 'function)

(defgeneric make-web-application (tbnl:easy-ssl-acceptor tbnl:easy-acceptor web-configuration)
  (:documentation "Input: hunchentoot easy-ssl-acceptor, easy-acceptor, web-configuration (default settings) object. Output web-application object."))

(defgeneric web-application-shell (web-configuration)
  (:documentation "Use this to start the web application."))

(defclass ssl-client-cert-acceptor (tbnl:easy-ssl-acceptor) ())

(defclass http-to-https-acceptor (tbnl:easy-acceptor)
  ((%ssl-port
    :reader ssl-port
    :initarg :ssl-port)))

(define-condition cert-file-missing (file-error)
  ()
  (:report (lambda (condition stream) (format stream "Unable to find certificate folder: ~A." (file-error-pathname condition)))))
