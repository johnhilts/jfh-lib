(in-package #:jfh-web-server)

;; TODO: should this part go into "internal"? #-start-#
(defmethod tbnl-handle-request-OLD :around ((tbnl:*acceptor* ssl-client-cert-acceptor) (tbnl:*request* tbnl:request))
  ;; only putting the fingerprint in session for testing purposes - the real code WILL NOT DO THAT
  ;; 2024-12-09 - this is specialized to SSL-CLIENT-CERT-ACCEPTOR, so that's how to control whether this method gets invoked in the first place
  ;; (let ((*break-on-signals* 'error))
    (let ((client-id ;; 'abc-123;;
            (cl+ssl:certificate-fingerprint (tbnl:get-peer-ssl-certificate)))
          (the-session (tbnl:start-session)))
      (setf (tbnl:session-value 'the-client-key the-session) client-id)
      ;; this needs to be replaced with a call to link the fingerprint to a 
      (setf (tbnl:session-value 'the-session the-session) "me@here.com") ;; "4061893-13726-7450288-41758") 
      ;; (my-log (format nil "ClientID,SessionExpiration,CookieValue~%"))
      (my-log (format nil "From H-R: ~A,~A~%" (tbnl:session-value 'the-client-key the-session) (tbnl:session-cookie-value the-session))))
    (when (next-method-p)
      (call-next-method)))
  ;; (break) TODO - why was this here??
;; )

(defun can-skip-certificate-auth ()
  (or
   (string=
    "/robots.txt"
    (tbnl:request-uri tbnl:*request*))
   (string=
    (format nil "~W" (cffi:null-pointer))
    (format nil "~W" (tbnl:get-peer-ssl-certificate)))))

;; TODO: should this part go into "internal"? #-start-#
;; TODO: whether this even runs should be based on configuration
(defmethod tbnl:handle-request :around ((tbnl:*acceptor* ssl-client-cert-acceptor) (tbnl:*request* tbnl:request))
  (unless (can-skip-certificate-auth)
    (let* ((client-id (cl+ssl:certificate-fingerprint (tbnl:get-peer-ssl-certificate)))
           (user-identifier (make-instance 'jfh-user:application-user-fingerprint :user-fingerprint client-id)))
      (format t "Link fingerprint to session using: ~A" client-id)
      (jfh-web-server:fetch-or-create-user-session user-identifier)))
  (when (next-method-p)
    (call-next-method)))

(defmethod tbnl:process-connection :around ((tbnl:*acceptor* ssl-client-cert-acceptor) (socket t))
  ;; (let ((*break-on-signals* 'error))
    (handler-bind
        ((error
           (lambda (cond)
             (format t "Error while processing the connection: ~A - caught by *ME* in the AROUND method!" cond)
             (return-from tbnl:process-connection))))
      (call-next-method))) ;; )

(defmethod tbnl:process-connection :after ((tbnl:*acceptor* ssl-client-cert-acceptor) (socket t))
  ;; (let ((*break-on-signals* 'error))
    (handler-bind
        ((error
           (lambda (cond)
             (break)
             (format t "Error while processing the connection: ~A - caught by *ME* in the AFTER method!" cond)
             (return-from tbnl:process-connection))))
      (when (next-method-p)
        (call-next-method)))) ; )

;; (defparameter *jfh/stolen-acceptor* nil)

(defmethod tbnl:initialize-connection-stream ((acceptor ssl-client-cert-acceptor) stream)
  "This function must return the stream to use. In this method, the stream is associated with CA and server certificates that will recognize client certificates from the same CA."
  ;; (let ((*break-on-signals* 'error))

    ;; attach SSL to the stream if necessary
    (let ((cert-path (get-cert-path)))
      (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-peer+
                                      :verify-depth 1
                                      :verify-location (format nil "~A/ca.crt" cert-path)
                                      :certificate-chain-file (format nil "~A/ca.crt" cert-path)
                                      )))
        (print "make server stream ...")
        (cl+ssl:with-global-context (ctx :auto-free-p t)
          (let ((server-stream (cl+ssl:make-ssl-server-stream
                                stream
                                :certificate (format nil "~A/server.crt" cert-path)
                                :key (format nil "~A/server.key" cert-path))))
            (setf *client-cert-missing* nil) ;; maybe this should be server-cert-missing??
            (handler-bind
                ((sb-sys:memory-fault-error
                   (lambda (c)
                     (format t "~&Error signaled: ~A~%" c)
                     (format t "~&Error context: ~A~%" (sb-sys:system-condition-context c))
                     (invoke-restart 'client-cert-missing)))
                 (error ;; OK to remove this clause
                   (lambda (cond)
                     (format t "Error while initializing connection stream: ~A" cond)
                     (return-from tbnl:initialize-connection-stream))))
              (restart-case
                  (let*
                      ((client-certificate (cl+ssl:ssl-stream-x509-certificate server-stream))
                       (client-cert-fingerprint (cl+ssl:certificate-fingerprint client-certificate :sha256))
                       (certificate-not-before-time (cl+ssl:certificate-not-before-time client-certificate))
                       (certificate-not-after-time (cl+ssl:certificate-not-after-time client-certificate))
                       (certificate-subject-common-names (cl+ssl:certificate-subject-common-names client-certificate)))
                    (format t "~&Client Cert: ~A, ~%fingerprint: ~A~%Not before time: ~A~%Not after time: ~A~%Subject common names: ~A~%"
                            client-certificate client-cert-fingerprint certificate-not-before-time certificate-not-after-time certificate-subject-common-names))
                (client-cert-missing ()
                  :report "No client certificate provided by the user."
                  (setf *client-cert-missing* t)
                  nil)))
            server-stream))))) ;;)

(defmethod tbnl:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  ;; (let ((*break-on-signals* 'error))
    (if (ssl-port acceptor)
        (tbnl:redirect (tbnl:request-uri request)
                       :port (ssl-port acceptor)
                       :protocol :https)
        (when (next-method-p)
          (call-next-method acceptor request)))) ; )

;; TODO: should this part go into "internal"? #-end-#

(defmethod print-object ((web-configuration web-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (web-configuration stream :type t)
    (with-accessors
          ((http-port http-port)
           (ssl-port ssl-port)
	   (static-root static-root)
           (accept-client-cert accept-client-cert))
        web-configuration
      (format stream
              "~:[~:;HTTP Port: ~:*~D, ~]~:[~:;SSL Port: ~:*~D, ~]Static root path: ~S, Accepts Client Cert: ~:[false~;true~]"
              http-port ssl-port static-root accept-client-cert))))

(defmethod print-object ((web-application web-application) stream)
  "Print web application."
  (print-unreadable-object (web-application stream :type t)
    (with-accessors ((hunchentoot-ssl-acceptor hunchentoot-ssl-acceptor) (hunchentoot-acceptor hunchentoot-acceptor) (web-configuration web-configuration))
        web-application
      (format stream
	      "Hunchentoot SSL Acceptor: ~a, Hunchentoot Acceptor: ~a, Configuration: ~a" hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))))

(defmethod start-hunchentoot ((web-configuration web-configuration))
  "start or re-start the hunchentoot web server"
  (flet ((make-acceptor-instances ()
           (with-accessors ((ssl-port ssl-port) (http-port http-port) (cert-path cert-path)) web-configuration
             (let ((server-key-path (format nil "~A/server.key" cert-path))
                   (server-crt-path (format nil "~A/server.crt" cert-path))
                   (ssl-acceptor-type (if (accept-client-cert web-configuration) 'ssl-client-cert-acceptor 'tbnl:easy-ssl-acceptor)))
               (values
                (make-instance ssl-acceptor-type :port ssl-port :ssl-privatekey-file server-key-path :ssl-certificate-file server-crt-path)
                (make-instance 'http-to-https-acceptor :port http-port :ssl-port ssl-port)))))
         (start-hunchentoot-by-http-protocol-type (acceptor-instance acceptor-type) ;; TODO: "acceptor-instance" is redundant!!
           (prog1
               (restart-case (tbnl:start acceptor-instance)
	         (skip-hunchentoot-start ()
                   :report "Skip starting Web Server (hunchentoot)."
                   (ecase acceptor-type
                     (ssl (hunchentoot-ssl-acceptor *web-application*))
                     (http (hunchentoot-acceptor *web-application*))))
	         ;; (use-different-port ()
	         ;;   :report "Use a different port - will increment by 1 from the configured port number."
	         ;;   ;; (format nil "Use a different port - change from ~d to ~d" (tbnl:acceptor-port acceptor-instance) (1+ (tbnl:acceptor-port acceptor-instance))))
	         ;;   (with-accessors ((http-port http-port) (ssl-port ssl-port)) web-configuration
	         ;; 	(start-hunchentoot (make-web-configuration (if ssl-port (1+ ssl-port) nil) (if http-port (1+ http-port) nil)))))
	         )
             (format t "~&instance: ~A~%" acceptor-instance))))
    (multiple-value-bind (ssl-acceptor-instance acceptor-instance)
        (make-acceptor-instances)
      (prog1
	  (list
           (start-hunchentoot-by-http-protocol-type ssl-acceptor-instance 'ssl)
           (start-hunchentoot-by-http-protocol-type acceptor-instance 'http))
        (format t "~&ssl: ~A, reg: ~A~%" ssl-acceptor-instance acceptor-instance)
	(format t "~&hunchentoot started~%")))))

(defmethod make-web-application ((hunchentoot-ssl-acceptor tbnl:easy-ssl-acceptor) (hunchentoot-acceptor tbnl:easy-acceptor) (web-configuration web-configuration))
  "Constructor for web-application - handles all parameters."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defmethod make-web-application ((hunchentoot-ssl-acceptor tbnl:easy-ssl-acceptor) (hunchentoot-acceptor (eql nil)) (web-configuration web-configuration))
  "Constructor for web-application - accepts nil for http."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defmethod make-web-application ((hunchentoot-ssl-acceptor (eql nil)) (hunchentoot-acceptor tbnl:easy-acceptor) (web-configuration web-configuration))
  "Constructor for web-application - accepts nil for ssl."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defmethod start-web-app ((web-configuration web-configuration))
  "Input: web-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot."
  (setf tbnl:*session-max-time* (* 24 7 60 60))
  (setf tbnl:*rewrite-for-session-urls* nil)
  (add-static-content-handlers)
  (destructuring-bind
      (ssl-acceptor acceptor)
      (start-hunchentoot web-configuration)
    (format t "~&ssl: ~A, reg: ~A~%" ssl-acceptor acceptor)
    (make-web-application ssl-acceptor acceptor web-configuration)))
;; how to find: (find-method #'start-web-app nil (list (find-class 'application-configuration)))

(defmethod stop-hunchentoot ((web-application web-application))
  "Input: web-application. Stop hunchentoot web-server via the provided web-application object."
  (flet ((stop-hunchentoot-by-http-protocol-type (acceptor)
           (restart-case (tbnl:stop acceptor)
             (skip-hunchentoot-stop ()
               :report "Skip stopping Web Server (hunchentoot)."
               acceptor))))
    (stop-hunchentoot-by-http-protocol-type (hunchentoot-acceptor web-application))
    (stop-hunchentoot-by-http-protocol-type (hunchentoot-ssl-acceptor web-application))))

(defmethod stop-web-app ((web-application web-application))
  "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released"
  (stop-hunchentoot web-application)
  '#:web-app-stopped)

(defmethod jfh-configuration:bind-configuration ((type (eql 'web)))
  "Input: the type, web. Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."
  (let ((web-configuration (make-web-configuration)))
    (setf *web-configuration* web-configuration)
    web-configuration))

(defmethod jfh-configuration:get-configuration ((type (eql 'web)))
  "Input: type such as 'app, 'remoting, or 'web. Output: configuration object. Configuration objects are NOT in an inheritance hierarchy."
  *web-configuration*)

(defmethod web-application-shell ((web-configuration web-configuration))
  "Use this to start the web application."
  (setf *web-application* (start-web-app web-configuration)))
