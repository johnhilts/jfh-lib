;;;; protocol for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defgeneric login-page (redirect-back-to)
  (:documentation "Input: URL. Redirect back to the given URL once logged in."))

(defgeneric signup-page ()
  (:documentation "No Input."))

(defgeneric on-successful-auth () ;; was ON-AUTH-HOOK
  (:documentation "Run this when auth is successful."))

(defgeneric show-auth-failure ()
  (:documentation "Generate page contents to display when auth fails."))

;; todo - replace this with JFH-WEB-SERVER:FETCH-OR-CREATE-USER-SESSION
(defgeneric establish-user-session (application-user)
  (:documentation "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."))

(defclass totp-info (jfh-store:user-settings jfh-security:aes) ()
  (:documentation "TOTP key for a user."))

(defgeneric save-totp-info (totp-info))

(defgeneric get-totp-info (jfh-user:application-user-identifier))
