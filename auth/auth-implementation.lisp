;;;; functions for auth related concerns. 
(cl:in-package #:jfh-auth)

(defparameter *session-user-map* (make-hash-table))

(defparameter *mfa-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defparameter *totp-checks* (make-hash-table :test #'equal) "Track TOTP checks by user")

(defparameter *webauthn-checks* (make-hash-table :test #'equal) "Track MFA checks by user")

(defvar *auth-configuration*)

(defun mfa-check (user-id mfa-checks-by-user test-type)
  "Generalized logic for determining if a user needs to be prompted for MFA."
  (let* ((last-mfa-check (gethash user-id mfa-checks-by-user 'not-found))
         (mfa-check-not-found (eql 'not-found last-mfa-check))
         (last-mfa-check-expired (or
                                  mfa-check-not-found
                                  (let ((test-threshold (mfa-test-threshold test-type)))
                                    (case test-type
                                      (mfa-time-test
                                       (>
                                        (- (get-universal-time) last-mfa-check)
                                        test-threshold))
                                      (mfa-count-test
                                       (> last-mfa-check test-threshold)))))))
    (or
     mfa-check-not-found
     last-mfa-check-expired)))

(defun mfa-test-type (enabled-mfa-schemes mfa-type)
  "Determine MFA test type by number of enabled MFA checks. Assuming this isn't called when no MFA checks are enabled."
  (case (length enabled-mfa-schemes)
    (1
     'mfa-time-test)
    (otherwise
     (case mfa-type
       (totp-mfa
        'mfa-count-test)
       (webauthn-mfa
        'mfa-time-test)))))

(defun mfa-test-threshold (test-type)
  "Get test threshold by test type."
   (if (eq 'mfa-time-test test-type) (* 60 (mfa-minute-threshold *auth-configuration*)) (mfa-count-threshold *auth-configuration*)))

(defun needs-totp-check (user-id enabled-mfa-schemes)
  (let ((test-type (mfa-test-type enabled-mfa-schemes 'totp-mfa)))
    (mfa-check user-id *totp-checks* test-type)))

(defun needs-webauthn-check (user-id enabled-mfa-schemes)
  (let ((test-type (mfa-test-type enabled-mfa-schemes 'webauthn-mfa)))
    (mfa-check user-id *webauthn-checks* test-type)))

(defun renew-mfa-check (user-id enabled-mfa-schemes mfa-checks mfa-type)
  "Renew mfa-check."  
  (case (mfa-test-type enabled-mfa-schemes mfa-type)
    (mfa-time-test
     ;; sliding expiration time
     (setf (gethash user-id mfa-checks) (get-universal-time)))
    (mfa-count-test
     (incf (gethash user-id mfa-checks)))))

(defmethod jfh-security:encrypt ((totp-info totp-info) &optional key)
  (let ((encryption-key (or key (jfh-security:fetch-key))))
    (call-next-method totp-info encryption-key)))

(defmethod jfh-security:decrypt ((totp-info totp-info) &optional key)
  (let ((encryption-key (or key (coerce (jfh-security:fetch-key) '(vector (unsigned-byte 8)))))) ;; TODO can we get rid of COERCE?
    (call-next-method totp-info encryption-key)))

(defmethod save-totp-info ((totp-info totp-info))
  (jfh-store:save-object totp-info))

(defun make-hash-table-from-list (list)
  (let ((ht (make-hash-table :test #'eq :size 8)))
    (loop for item in list
          do
             (let* ((raw-value (cadr item))
                    (value (if (simple-vector-p raw-value)
                               (coerce raw-value '(simple-array (unsigned-byte 8) (*)))
                               raw-value)))
               (setf (gethash (car item) ht) value)))
    ht))

(defmethod initialize-instance :after ((webauthn-info webauthn-info) &key)
  "Initializations:
- Transform list to hash table"
  (let ((public-key #1=(slot-value webauthn-info '%public-key)))
    (when (eql 'cons (type-of public-key))
      (setf #1#
            (make-hash-table-from-list public-key)))))

(defmethod initialize-instance :after ((webauthn-info-readable webauthn-info-readable) &key)
  "Initializations:
- Transform hash table to k-v list"
  (let ((public-key-readable (if (slot-boundp webauthn-info-readable '%public-key-readable) #1=(slot-value webauthn-info-readable '%public-key-readable) nil)))
    (when (and public-key-readable (eql 'hash-table (type-of public-key-readable)))
      (setf #1#
            (loop for k being the hash-key
                    using (hash-value v) of public-key-readable
                  collect
                  (list k v))))))

(defmethod print-object ((auth-configuration auth-configuration) stream)
  "Print auth configuration."
  (print-unreadable-object (auth-configuration stream :type t)
    (with-accessors
          ((enable-totp enable-totp)
           (enable-webauthn enable-webauthn)
           (timeout timeout)
           (mfa-minute-threshold mfa-minute-threshold)
           (mfa-count-threshold mfa-count-threshold))
        auth-configuration
      (format stream
              "Enable TOTP: ~:[false~;true~], Enable WebAuthN: ~:[false~;true~], Timeout: ~D, MFA Minute Threshold: ~D, MFA Count Threshold: ~D"
              enable-totp enable-webauthn timeout mfa-minute-threshold mfa-count-threshold))))

(defmethod make-auth-configuration ()
  (jfh-store:make-instance* 'auth-configuration))

(defmethod jfh-configuration:bind-configuration ((type (eql 'auth)))
  "Input: the type, auth. Output: a configuration object. Configuration objects are NOT in an inheritance hierarchy."
  (let ((auth-configuration (make-auth-configuration)))
    (setf *auth-configuration* auth-configuration)
    auth-configuration))

(defmethod jfh-configuration:get-configuration ((type (eql 'auth)))
  "Input: type such as 'app, 'remoting, or 'web. Output: configuration object. Configuration objects are NOT in an inheritance hierarchy."
  *auth-configuration*)
