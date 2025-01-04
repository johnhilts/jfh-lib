;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defun get-certificate-fingerprint-from-file (certificate-folder unsafe-email)
  "Inputs: CERTIFICATE-FOLDER: fully qualified folder path of DER certificate. UNSAFE-EMAIL: the normal format for the client email. Will be regexed into a \"safe email\".
Output: the certificate fingerprint as a decimal array."
  (let* ((safe-email (cl-ppcre:regex-replace "@" unsafe-email "_"))
         (certificate-file-path (format nil "~A/client-~A.der" certificate-folder safe-email)))
    ;; NOTE: CL+SSL:DECODE-CERTIFICATE-FROM-FILE only works with .der certificates.
    (cl+ssl:certificate-fingerprint (cl+ssl:decode-certificate-from-file certificate-file-path :format :der))))
