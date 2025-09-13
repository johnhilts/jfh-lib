(in-package #:jfh-web-server)

;; TODO: should this part go into "internal"? #-start-#
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
      (format t "Link fingerprint to session using: ~A~%" client-id)
      (jfh-web-server:fetch-or-create-user-session user-identifier)))
  (when (next-method-p)
    (call-next-method)))

(defmethod tbnl:process-connection :around ((tbnl:*acceptor* ssl-client-cert-acceptor) (socket t))
  (handler-bind
      ((sb-bsd-sockets:socket-error
         (lambda (e)
           (format t "~&[process-connection :around] Socket error: ~A~%" e)
           (return-from tbnl:process-connection)))
       (stream-error
         (lambda (e)
           (format t "~&[process-connection :around] Stream error: ~A~%" e)
           (return-from tbnl:process-connection)))
       (type-error
         (lambda (e)
           (format t "~&[process-connection :around] Type error: ~A~%" e)
           (return-from tbnl:process-connection)))
       (cl+ssl::ssl-error-ssl
         (lambda (e)
           (format t "~&[process-connection :around] SSL error SSL: ~A~%" e)
           (return-from tbnl:process-connection))))
    (call-next-method)))

(defmethod tbnl:process-connection :after ((tbnl:*acceptor* ssl-client-cert-acceptor) (socket t))
  ;; (let ((*break-on-signals* 'error))
    (handler-bind
        ((error
           (lambda (cond)
             (break)
             (format t "Error while processing the connection: ~A - caught by *ME* in the AFTER method!~%" cond)
             (return-from tbnl:process-connection))))
      (when (next-method-p)
        (call-next-method)))) ; )


(defmethod tbnl:initialize-connection-stream ((acceptor ssl-client-cert-acceptor) stream)
  "Initialize SSL connection stream with client cert verification. If the client fails to present a valid cert, abort quietly."
  (when (typep stream 'stream)
    (let* ((cert-path (get-cert-path))
           (ctx (cl+ssl:make-context
                 :verify-mode cl+ssl:+ssl-verify-peer+
                 :verify-depth 1
                 :verify-location (format nil "~A/ca.crt" cert-path)
                 :certificate-chain-file (format nil "~A/ca.crt" cert-path)
                 :options (list cl+ssl::+SSL-OP-ALL+ cl+ssl::+SSL-OP-IGNORE-UNEXPECTED-EOF+)
                 :disabled-protocols (list cl+ssl::+SSL-OP-NO-SSLV2+ cl+ssl::+SSL-OP-NO-SSLV3+ cl+ssl::+SSL-OP-NO-TLSv1+ cl+ssl::+SSL-OP-NO-TLSv1-1+ cl+ssl::+SSL-OP-NO-TLSv1-3+))))
      (cl+ssl:with-global-context (ctx :auto-free-p t)
        (let ((server-stream (cl+ssl:make-ssl-server-stream
                              stream
                              :certificate (format nil "~A/server.crt" cert-path)
                              :key (format nil "~A/server.key" cert-path))))
          ;; Attempt to extract client certificate
          (handler-case
              (let ((client-cert (cl+ssl:ssl-stream-x509-certificate server-stream)))
                (unless client-cert
                  ;; No client cert presented
                  (format t "~&No client certificate presented. Connection ignored.~%"))
                server-stream)
            (cl+ssl::ssl-error (e)
              ;; SSL handshake failed—likely due to missing or invalid client cert
              (format t "~&[initialize-connection-stream] SSL error during handshake: ~A~%" e)
              server-stream)
            (sb-bsd-sockets:socket-error (e)
              ;; Catch low-level stream/socket errors
              (format t "~&[initialize-connection-stream] Socket error: ~A~%" e)
              server-stream)
            (stream-error (e)
              (format t "~&[initialize-connection-stream] Stream error: ~A~%" e)
              server-stream)
            (error (e)
              ;; Catch-all for other unexpected errors
              (format t "~&[initialize-connection-stream] Unexpected error during stream initialization: ~A~%" e)
              server-stream)))))))

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
