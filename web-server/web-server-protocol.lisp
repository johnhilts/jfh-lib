(in-package #:jfh-web-server)

(defclass web-configuration (jfh-store:config-data)
  ((%http-port
    :reader http-port
    :initarg :http-port)
   (%ssl-port
    :reader ssl-port
    :initarg :ssl-port)
   (%static-root
    :reader static-root
    :initarg :static-root)
   (%cert-path
    :reader cert-path
    :initarg :cert-path)
   (%accept-client-cert
    :reader accept-client-cert
    :initarg :accept-client-cert)
   (%site-registrable-domain
    :reader site-registrable-domain
    :initarg :site-registrable-domain
    :documentation "relying party's ID. Example: site.com")
   (%site-display-name
    :reader site-display-name
    :initarg :site-display-name
    :documentation "Display on the front end as site's name. Can also use for replying party's name.")
   (%site-origin
    :reader site-origin
    :initarg :site-origin
    :documentation "Example: https://www.site.com; can also use as webauthn expected origin."))
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
    :initarg :web-configuration)
   (%auth-configuration
    :reader jfh-auth:auth-configuration
    :initarg :auth-configuration))
  (:documentation "Web application."))

(defgeneric start-hunchentoot (web-application)
  (:documentation "Input: web-application. Start hunchentoot web-server with the provided configuration settings."))

(defgeneric start-web-app (web-configuration auth-configuration)
  (:documentation "Input: web-configuration and auth-configuration object. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."))
;; (documentation 'start-web-app 'function)

(defgeneric stop-hunchentoot (web-application)
  (:documentation "Input: web-application. Stop hunchentoot web-server via the provided web-application object."))

(defgeneric stop-web-app (web-application)
  (:documentation "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released."))
;; (documentation 'stop-web-app 'function)

(defgeneric make-web-application (tbnl:easy-ssl-acceptor tbnl:easy-acceptor web-configuration auth-configuration)
  (:documentation "Input: hunchentoot easy-ssl-acceptor, easy-acceptor, web-configuration (default settings), auth-configuration object. Output web-application object."))

(defgeneric web-application-shell (web-configuration auth-configuration)
  (:documentation "Use this to start the web application."))

(defclass ssl-client-cert-acceptor (tbnl:easy-ssl-acceptor) ())

(defclass http-to-https-acceptor (tbnl:easy-acceptor)
  ((%ssl-port
    :reader ssl-port
    :initarg :ssl-port)))

(define-condition cert-file-missing (file-error)
  ()
  (:report (lambda (condition stream) (format stream "Unable to find certificate folder: ~A." (file-error-pathname condition)))))

(defgeneric fetch-or-create-user-session (user-identifier) 
  (:documentation "Fetch an existing or create a new user session based on the user-identifier."))

(defgeneric mfa-enabled-schemes (configuration) 
  (:documentation "Determine whether MFA is enabled, and what types. Return list of supported and enabled MFA schemes."))

(defgeneric need-mfa-check (request user-id enabled-mfa-schemes) 
  (:documentation "Determine whether the given user ID needs an MFA check, and what types. Return list of MFA schemes to use in an MFA check."))

(defgeneric prompt-mfa (request user-id need-mfa-check enabled-mfa-schemes) 
  (:documentation "Trigger prompt for an MFA check for the given user ID; this links to the UI."))

