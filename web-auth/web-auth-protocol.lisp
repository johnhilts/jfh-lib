;;;; protocol for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defgeneric login-page (redirect-back-to)
  (:documentation "Input: URL. Redirect back to the given URL once logged in."))

(defgeneric signup-page ()
  (:documentation "No Input."))

;; todo - replace this with JFH-WEB-SERVER:FETCH-OR-CREATE-USER-SESSION
(defgeneric establish-user-session (application-user)
  (:documentation "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."))

(defgeneric get-totp-info (jfh-user:application-user-identifier))

(defgeneric get-webauthn-info (jfh-user:application-user-identifier))

(defclass webauthn-configuration (jfh-auth:auth-configuration jfh-web-server:web-configuration) ()
  (:documentation "Convenience class with all the settings used by Webauthn. Meant to be populated by objects that implement its parent types."))

(defgeneric make-webauthn-configuration (auth-configuration web-configuration)
  (:documentation "Output: WEBAUTHN-CONFIGURATION object."))

(defclass application-user-webauthn-credentials (jfh-user:application-user-identifier)
  ((%user-credential-id
    :reader user-credential-id
    :initarg :user-credential-id))
  (:documentation "User's Credential ID. Meant for what a user interactively inputs for a Credential ID via webauthn / biometrics."))

(defclass user-webauthn-credential-index-entry (jfh-auth:user-webauthn-credential-index application-user-webauthn-credentials)
  ()
  (:documentation "User index entry. Link User ID to persisted data."))
