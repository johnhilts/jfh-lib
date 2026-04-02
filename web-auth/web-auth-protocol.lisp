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

(defgeneric get-webauthn-info (jfh-user:application-user-identifier))

(defclass webauthn-challenge (jfh-store:user-settings)
  ((%challenge :accessor challenge :initarg :challenge)))

(defclass webauthn-info-readable (jfh-store:user-settings)
  ((%credential-id :accessor credential-id :initarg :credential-id)
   (%public-key-readable :accessor public-key-readable :initarg :public-key-readable)
   (%sign-count :accessor sign-count :initarg :sign-count)))

(defclass webauthn-info (webauthn-info-readable)
  ((%public-key :accessor public-key :initarg :public-key)))

(defclass user-webauthn-credential-index (jfh-store:user-index)
  ((%user-credential-id :reader user-credential-id :initarg :user-credential-id)))

(defclass application-user-webauthn-credentials (jfh-user:application-user-identifier)
  ((%user-credential-id
    :reader user-credential-id
    :initarg :user-credential-id))
  (:documentation "User's Credential ID. Meant for what a user interactively inputs for a Credential ID via webauthn / biometrics."))

(defclass user-webauthn-credential-index-entry (user-webauthn-credential-index application-user-webauthn-credentials)
  ()
  (:documentation "User index entry. Link User ID to persisted data."))
