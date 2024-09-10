;;;; functions to handle auth related concerns. Depends on hunchentoot.
(cl:in-package #:jfh-web-auth)

(tbnl:define-easy-handler (authenticate-handler :uri "/auth") (user-login password redirect-back-to)
  (let* ((user-info (jfh-user:get-secure-user-info user-login))
         (authed (and user-info
                      (string=
                       (jfh-user:user-password user-info)
                       (jfh-user:hash-password password))))) ;; NOTE: AND returns the result of the last form
    (handle-auth-result authed user-info redirect-back-to)))

(tbnl:define-easy-handler (authenticate-cert-handler :uri "/auth-cert") (user-fingerprint redirect-back-to)
  (let* ((user-info (jfh-user:get-secure-user-info user-fingerprint))
         (authed (not (null user-info))))
    (handle-auth-result authed user-info redirect-back-to)))

(defun handle-auth-result (authed user-info redirect-back-to) ;; auth'd
  "handle auth success and failure"
  (if authed
      (progn
        (establish-user-session user-info)
        (on-successful-auth) ;; 'web-app:on-auth-hook
        (tbnl:redirect redirect-back-to)) ;; to test this, need to mock *request* and *acceptor*
      (show-auth-failure))) ;; 'web-app:show-auth-failure

(tbnl:define-easy-handler (login-page-handler :uri "/login") (redirect-back-to)
  (funcall (login-page *web-auth-pages*) redirect-back-to))

(tbnl:define-easy-handler (signup-page-handler :uri "/signup") ()
  (funcall (signup-page *web-auth-pages*)))

(tbnl:define-easy-handler (logout-page-handler :uri "/logout") ()
  "logout endpoint"
  (format t
	  "~&www-authorization: ~A, authorization: *** ~A ***~%"
	  (tbnl:header-out :www-authenticate)
	  (tbnl:header-out "authorization"))
  (remhash (tbnl:session-value 'the-session) (session-user-map *web-auth-pages*))
  (tbnl:delete-session-value 'the-session)
  (setf (tbnl:header-out :www-authenticate) nil)
  (tbnl:redirect "/login"))
