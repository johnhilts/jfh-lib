;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defmethod jfh-web-server:fetch-or-create-user-session ((user-identifier jfh-user:application-user-login))
  "Establish the user session in Hunchentoot's session apparatus + in cookies."
  (let ((session-token (jfh-utility:generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token jfh-auth:*session-user-map*) (jfh-store:user-id (jfh-user:get-secure-user-info user-identifier)))))

(defmethod jfh-web-server:fetch-or-create-user-session ((user-identifier jfh-user:application-user-fingerprint))
  "Establish the user session in Hunchentoot's session apparatus + in cookies."
  (if (tbnl:session-value 'the-session-key)
      (tbnl:session-value 'the-session-key)
      (setf
       (tbnl:session-value 'the-session-key)
       (handler-bind
           ((jfh-store:no-data-match
              (lambda (c)
                (format t "~&No matching data found! CLASS-NAME: ~A~%USER ID: ~A~%WHERE: ~A~%" (jfh-store:the-class-name c) (jfh-store:user-id c) (jfh-store:where c))
                (return-from jfh-web-server:fetch-or-create-user-session nil))))
         (let ((user-id (jfh-store:user-id (jfh-user:get-secure-user-info user-identifier))))
           (remhash user-id jfh-auth:*mfa-checks*)
           user-id)))))

(defmethod jfh-web-server:mfa-enabled-schemes ((configuration jfh-auth:auth-configuration))
  "Determine whether MFA is enabled, and what types. Return list of supported and enabled MFA schemes."
  (remove-if-not #'identity
                 (list
                  (if (jfh-auth:enable-totp configuration) 'jfh-auth:totp-mfa nil) 
                  (if (jfh-auth:enable-webauthn configuration) 'jfh-auth:webauthn-mfa nil))))

(defparameter *totp-checks* (make-hash-table :test #'equal) "Track TOTP checks by user")

(defparameter *webauthn-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defmethod jfh-web-server:need-mfa-check ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Determine whether the given user ID needs an MFA check, and what types. Return list of MFA schemes to use in an MFA check."
  (let ((mfa-setup-in-progress (or
                                (search "totp-setup" (tbnl:script-name tbnl:*request*))
                                (search "b-registration" (tbnl:script-name tbnl:*request*))))
        (mfa-in-progress (or
                          (search "-mfa" (tbnl:script-name tbnl:*request*))
                          (search "-totp" (tbnl:script-name tbnl:*request*))
                          (search "webauthn" (tbnl:script-name tbnl:*request*))
                          (search "biometrics" (tbnl:script-name tbnl:*request*))))
        (mfa-can-skip (search "/styles.css" (tbnl:script-name tbnl:*request*))))
    (when (and
           user-id
           enabled-mfa-schemes
           (not mfa-setup-in-progress)
           (not mfa-in-progress)
           (not mfa-can-skip))
      (remove-if-not #'identity
                     (list 
                      (if (and
                           (member 'jfh-auth:webauthn-mfa enabled-mfa-schemes)
                           (jfh-auth:needs-webauthn-check user-id enabled-mfa-schemes)) 
                          'jfh-auth:webauthn-mfa
                          'nil)
                      (if (and 
                           (member 'jfh-auth:totp-mfa enabled-mfa-schemes)
                           (jfh-auth:needs-totp-check user-id enabled-mfa-schemes))
                          'jfh-auth:totp-mfa
                          nil))))))

(defmethod jfh-auth:get-totp-info ((application-user-id jfh-user:application-user-id))
  (let ((user-id (jfh-store:user-id application-user-id)))
    (jfh-store:make-instance* 'jfh-auth:totp-info :user-id user-id)))

(defmethod get-webauthn-info ((application-user-id jfh-user:application-user-id))
  (let ((user-id (jfh-store:user-id application-user-id)))
    (jfh-store:make-instance* 'jfh-auth:webauthn-info-readable :user-id user-id)))

(defun needs-totp-setup (user-id)
  "Check whether user needs TOTP setup. The check is based on whether the TOTP key is populated; it defaults to an empty string, so check using LENGTH is safe."
  (let ((totp-info (get-totp-info (make-instance 'jfh-user:application-user-id :user-id user-id))))
    (or (not totp-info) (zerop (length (jfh-security:cipher totp-info))))))

(defun needs-webauthn-setup (user-id)
  "Check whether user needs webauthn setup. The check is based on whether a WEBAUTHN-INFO-READABLE exists for the user. Default to an empty string, so using LENGTH is safe."
  (let ((webauthn-info (get-webauthn-info (make-instance 'jfh-user:application-user-id :user-id user-id))))
    (or (not webauthn-info) (zerop (length (jfh-auth:credential-id webauthn-info))))))

(defmethod jfh-auth:prompt-totp ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Redirect to TOTP prompt. The conditions are: 1. No recent MFA check."
  (if (needs-totp-setup user-id)
      (tbnl:redirect (format nil "/totp-setup?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*))))
      (tbnl:redirect (format nil "/prompt-totp?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  (jfh-auth:renew-mfa-check user-id enabled-mfa-schemes *totp-checks* 'jfh-auth:totp-mfa))

(defmethod prompt-webauthn ((tbnl:*request* tbnl:request) user-id enabled-mfa-schemes)
  "Redirect to WebAuthN prompt. The conditions are: 1. No recent MFA check."
  (format t "uri: ~A~%" (tbnl:request-uri tbnl:*request*))
  (if (needs-webauthn-setup user-id)
      (tbnl:redirect (format nil "/b-registration?return-url=~A" (tbnl:url-encode  (tbnl:request-uri tbnl:*request*))))
      (tbnl:redirect (format nil "/prompt-webauthn?return-url=~A" (tbnl:url-encode (tbnl:request-uri tbnl:*request*)))))
  
  (jfh-auth:renew-mfa-check user-id enabled-mfa-schemes *webauthn-checks* 'jfh-auth:webauthn-mfa))

(defmethod jfh-web-server:prompt-mfa ((tbnl:*request* tbnl:request) user-id need-mfa-check enabled-mfa-schemes)
  (cond
    ((member 'jfh-auth:totp-mfa need-mfa-check)
     (jfh-auth:prompt-totp tbnl:*request* user-id enabled-mfa-schemes))
    ((member 'jfh-auth:webauthn-mfa need-mfa-check)
     (prompt-webauthn tbnl:*request* user-id enabled-mfa-schemes))))

(defmethod tbnl:handle-request :after ((tbnl:*acceptor* jfh-web-server:ssl-client-cert-acceptor) (tbnl:*request* tbnl:request))
  (unless (jfh-web-server:can-skip-certificate-auth)
    (setf jfh-security:*key* (cl+ssl:certificate-fingerprint (tbnl:get-peer-ssl-certificate))))
  (when (next-method-p)
    (call-next-method)))

(defmethod jfh-user:get-user-info ((user-login application-user-webauthn-credentials))
  "Search for user info in file system."
  (let ((user-index-entry (jfh-store:make-instance* 'user-webauthn-credential-index-entry :where `(:user-credential-id ,(user-credential-id user-login)))))
    (jfh-store:make-instance* 'jfh-auth:webauthn-info-readable :user-id (jfh-store:user-id user-index-entry))))

(defmethod jfh-user:save-application-user ((application-user jfh-auth:webauthn-info-readable))
  "Input: webauthn-info-readable. Persist user's webauthn info."
  (jfh-store:save-object application-user))

(defmethod jfh-user:make-user-login-index-entry ((application-user jfh-auth:webauthn-info-readable))
  "Input: webauthn-info-readable. Output: user credential-ID index entry."
  (make-instance 'user-webauthn-credential-index-entry
		 :user-credential-id (jfh-auth:credential-id application-user)
		 :user-id (jfh-store:user-id application-user)))

(defmethod jfh-user:save-index ((application-user jfh-auth:webauthn-info-readable))
  (when (and
         (jfh-auth:credential-id application-user)
         (plusp (length (jfh-auth:credential-id application-user))))
    (jfh-store:save-object (jfh-user:make-user-login-index-entry application-user))))

(defmethod jfh-user:save-new-application-user ((application-user jfh-auth:webauthn-info-readable))
  "Input: webauthn-info-readable. Persist user's webauthn info."
  (jfh-user:save-index application-user)
  (jfh-user:save-application-user application-user))

(defmethod print-object ((webauthn-configuration webauthn-configuration) stream)
  "Print webauthn configuration."
  (print-unreadable-object (webauthn-configuration stream :type t)
    (with-accessors
          ((enable-webauthn jfh-auth:enable-webauthn)
           (site-origin jfh-web-server:site-origin)
           (site-display-name jfh-web-server:site-display-name)
           (site-registrable-domain jfh-web-server:site-registrable-domain)
           (timeout jfh-auth:timeout))
        webauthn-configuration
      (format stream
              "Enable WebAuthN: ~:[false~;true~], Origin: ~S, Display Name (RP Name) : ~S, Registrable Domain (RP ID) : ~S, Timeout: ~D"
              enable-webauthn site-origin site-display-name site-registrable-domain timeout))))

(defmethod make-webauthn-configuration ((auth-configuration jfh-auth:auth-configuration) (web-configuration jfh-web-server:web-configuration))
  (make-instance 'webauthn-configuration
                 :enable-webauthn (jfh-auth:enable-webauthn auth-configuration)
                 :site-origin (jfh-web-server:site-origin web-configuration)
                 :site-display-name (jfh-web-server:site-display-name web-configuration)
                 :site-registrable-domain (jfh-web-server:site-registrable-domain web-configuration)
                 :timeout (jfh-auth:timeout auth-configuration)))

(defmethod jfh-configuration:bind-configuration ((type (eql 'webauthn)))
  "Input: the type, webauthn. Output: a configuration object. (Most) configuration objects are NOT in an inheritance hierarchy."
  (let ((webauthn-configuration (make-webauthn-configuration (jfh-configuration:get-configuration 'jfh-auth:auth) (jfh-configuration:get-configuration 'jfh-web-server:web))))
    (setf jfh-auth:*webauthn-configuration* webauthn-configuration)
    webauthn-configuration))
