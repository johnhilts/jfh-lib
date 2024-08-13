;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defmethod use-web-auth ((web-auth-pages web-auth-pages))
  "Input: web-auth-pages. Use this to enable user auth in a web app."
  (setf *web-auth-pages* web-auth-pages))

(defmethod establish-user-session ((application-user jfh-user:application-user))
  "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."
  (let ((session-token (jfh-utility:generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token (session-user-map *web-auth-pages*)) (jfh-user:user-id application-user))))
