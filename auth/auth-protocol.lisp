;;;; protocol for auth related concerns. 
(cl:in-package #:jfh-auth)

(defgeneric on-auth-success ()
  (:documentation "Run this when auth is successful."))

(defgeneric on-auth-failure () ;; was show-auth-failure
  (:documentation "Run this when auth fails."))

(defclass totp-info (jfh-store:user-settings jfh-security:aes) ()
  (:documentation "TOTP key for a user."))

(defgeneric save-totp-info (totp-info))

(defgeneric prompt-totp (request user-id enabled-mfa-schemes) 
  (:documentation "Prompt for TOTP. Redirect to another page to prompt input."))

(defgeneric prompt-webauthn (request user-id enabled-mfa-schemes) 
  (:documentation "Prompt for WebAuthN. Redirect to another page to prompt input."))

(defclass webauthn-challenge (jfh-store:user-settings)
  ((%challenge :accessor challenge :initarg :challenge)))

(defclass webauthn-info-readable (jfh-store:user-settings)
  ((%credential-id :accessor credential-id :initarg :credential-id)
   (%public-key-readable :accessor public-key-readable :initarg :public-key-readable)
   (%sign-count :accessor sign-count :initarg :sign-count)))

(defclass webauthn-info (webauthn-info-readable)
  ((%public-key :accessor public-key :initarg :public-key)))

(defclass auth-configuration (jfh-store:config-data)
  ((%enable-totp :reader enable-totp :initarg :enable-totp)
   (%enable-webauthn :reader enable-webauthn :initarg :enable-webauthn)
   (%timeout :reader timeout :initarg :timeout)
   (%mfa-minute-threshold :reader mfa-minute-threshold :initarg :mfa-minute-threshold)
   (%mfa-count-threshold :reader mfa-count-threshold :initarg :mfa-count-threshold))
  (:documentation "Global settings for auth related settings."))

(defgeneric make-auth-configuration ()
  (:documentation "Output: AUTH-CONFIGURATION object."))

(defclass user-webauthn-credential-index (jfh-store:user-index)
  ((%user-credential-id :reader user-credential-id :initarg :user-credential-id)))
