;;;; functions to support C iterop
(cl:in-package #:jfh-auth)

(push
 (make-pathname :directory
                (subseq (format nil "~A~A~A"
                                jfh-globals:*jfh-app/home-folder*
                                jfh-globals:*jfh-app/web-app-root-folder*
                                jfh-globals:*jfh-app/cffi-root-folder*)
                        1))
 cffi:*foreign-library-directories*)

(cffi:define-foreign-library libauthn-es256
  (:linux "libauthn-es256.so") ;; TODO do we need both of these?
  (t "libauthn-es256.so"))

(cffi:use-foreign-library libauthn-es256)

(cffi:defcfun ("verify_es256" %verify-es256) :int
  (x :pointer) (x-len :size)
  (y :pointer) (y-len :size)
  (msg :pointer) (msg-len :size)
  (sig :pointer) (sig-len :size))

(defun %fill-foreign-from-bytes (ptr bytes)
  (loop for i from 0 below (length bytes)
        do (setf (cffi:mem-aref ptr :uint8 i)
                 (aref bytes i))))

(defun verify-es256 (x-bytes y-bytes msg-bytes sig-bytes)
  "Return T if signature is valid, NIL if invalid, or signal error on -1."
  (cffi:with-foreign-pointer (x-ptr (length x-bytes))
    (cffi:with-foreign-pointer (y-ptr (length y-bytes))
      (cffi:with-foreign-pointer (msg-ptr (length msg-bytes))
        (cffi:with-foreign-pointer (sig-ptr (length sig-bytes))
          (%fill-foreign-from-bytes x-ptr x-bytes)
          (%fill-foreign-from-bytes y-ptr y-bytes)
          (%fill-foreign-from-bytes msg-ptr msg-bytes)
          (%fill-foreign-from-bytes sig-ptr sig-bytes)
          (let ((rc (%verify-es256
                     x-ptr (length x-bytes)
                     y-ptr (length y-bytes)
                     msg-ptr (length msg-bytes)
                     sig-ptr (length sig-bytes))))
            (cond
              ((= rc 1) t)
              ((= rc 0) nil)
              (t (error "verify_es256: OpenSSL error (~A)" rc)))))))))
