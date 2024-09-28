;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defparameter *session-user-map* (make-hash-table))

(defmethod establish-user-session ((application-user jfh-user:application-user))
  "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."
  (let ((session-token (jfh-utility:generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token *session-user-map*) (jfh-user:user-id application-user))))
