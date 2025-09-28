(in-package #:jfh-security)

(defmethod initialize-instance :after ((aes aes) &key)
  "Initializations:
- Encrypt the plaintext then remove it. This is meant to prevent the plain text from being in memory."
  (let ((cipher #1=(slot-value aes '%cipher))
        (iv #2=(slot-value aes '%iv)))
    (when (zerop (length cipher))
      (setf #1# (encrypt aes))
      (setf #2# iv)
      (setf (slot-value aes '%plaintext) ""))
    (when (next-method-p)
      (call-next-method))))

(defmethod encrypt ((info aes) &optional key)
  (when (not key)
    (warn "encryption key not set."))
  
  (let* ((cipher-encrypt (ironclad:make-cipher :aes
                                               :key key
                                               :mode :cfb
                                               :initialization-vector (iv info)))
         (text (ironclad:ascii-string-to-byte-array (cl-base64:usb8-array-to-base64-string (ironclad:ascii-string-to-byte-array (plaintext info))))))

    (ironclad:encrypt-in-place cipher-encrypt text)

    (cl-base64:usb8-array-to-base64-string text)))

(defmethod decrypt ((info aes) &optional key)
  (when (not key)
    (warn "encryption key not set."))
  
  (let* ((salt (coerce  (iv info) '(vector (unsigned-byte 8)))) ;; TODO - I think we can drop COERCE
         (cipher (cl-base64:base64-string-to-usb8-array (cipher info)))
         (cipher-decrypt (ironclad:make-cipher :aes
                                               :key key
                                               :mode :cfb
                                               :initialization-vector salt)))
    (ironclad:decrypt-in-place cipher-decrypt cipher)
    cipher
    (cl-base64:base64-string-to-string
     (coerce (mapcar #'code-char (coerce cipher 'list)) 'string))))
