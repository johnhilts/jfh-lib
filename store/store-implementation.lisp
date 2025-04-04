(in-package #:jfh-store)

;; TODO - need to update
;; (defmethod print-object ((file-store file-store) stream)
;;   "Print file store."
;;   (print-unreadable-object (file-store stream :type t)
;;     (with-accessors ((path path) (label label)) file-store
;;       (format stream
;; 	      "Path: ~A, Label: ~A" path label))))

;; (defmethod initialize-instance :after ((user-data user-data) &key)
;;   "Initializations:
;; - Set the ID to a unique ID."
;;   (let ((data-id #1=(slot-value user-data '%data-id)))
;;     (when (zerop (length data-id))
;;       (setf #1# (jfh-utility:generate-unique-token)))))

;; TODO - need to update
(defun make-instance-list (class-name &key key)
  (let ((file-contents (get-data class-name :key key))) ;; TODO in the future, may replace GET-DATA with something that will READ-LINE instead of do 1 READ
    (loop for line in file-contents
          while line
          collect
          (apply #'make-instance class-name line))))

(defun get-index-full-file-name (where)
  (let ((index-file-name (derive-index-from-where where)))
    (when index-file-name
      (let ((directory (get-file-directory 'user-index)))
        (format nil "~A~A.sexp" directory index-file-name)))))

(defun read-from-index (index-full-file-name where)
  (let ((index-contents (fetch-or-create-data index-full-file-name)))
    (destructuring-bind
        (key value)
        where
      (let ((comparer (if (vectorp value) #'equal #'string=)))
        (find-if
         (lambda (e) (funcall comparer value (getf e key)))
         index-contents)))))

(defun interpolate-user-id-into (user-id content)
  (if (or
       (null user-id)
       (null content))
      content
      (if (and
           (listp content)
           (atom (car content))
           (not (getf content :user-id)))
          (append
           (list :user-id user-id)
           content)
          content)))

(defmethod make-instance* ((class-name symbol) &key where user-id)
  ;; parse WHERE, if it exists, to derive the suitable index
  ;; if no WHERE, or no index found, default to USER-INDEX
  ;; derive correct file path from index
  ;; get data "however" - one giant READ, or READ-LINE + primary index
  (labels ((choose-comparer (value) ;; TODO - replace this with methods / specialization
             (typecase value
               (vector #'equalp)
               (number #'=)
               (otherwise #'string=))))
    (flet ((get-file-contents ()
             (let* ((need-index-lookup (and (null user-id) where))
                    (index-full-file-name (and need-index-lookup (get-index-full-file-name where)))
                    (key-user-id (or user-id (and need-index-lookup (getf (read-from-index index-full-file-name where) :user-id))))
                    (full-file-name (get-full-file-name class-name (string-downcase (string class-name)) (lambda () key-user-id)))
                    (file-contents (fetch-or-create-data full-file-name)))
               (if where
                   (destructuring-bind (key value)
                       where ;; NOTE - assuming only 1 "clause"; TODO - support any number of clauses, or a lambda (easier!)
                     (let ((comparer (choose-comparer value)))
                       (interpolate-user-id-into key-user-id
                                                 (car
                                                  (remove-if-not
                                                   (lambda (e)
                                                     (funcall comparer value (getf e key))) ;; TODO - be more robust than STRING= and EQUALP
                                                   file-contents)))))
                   (interpolate-user-id-into key-user-id file-contents)))))
      (let ((file-contents (get-file-contents)))  ;; TODO - SIGNAL jfh-store:no-data-match if NIL
        (unless (null file-contents)
          ;; TODO - error if file-contents has multiple rows - you need to use WHERE
          (apply #'make-instance class-name file-contents))))))

(defmethod serialize-object->list ((object flat-file) accessors)
  "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used with 1 \"row\" of data."
  (loop for accessor in accessors
        nconc
        (list
         (intern (string accessor) (find-package 'keyword))
         (funcall accessor object))))

;; TODO - need to complete implementation
(defun prepend-data (full-file-name serialized-data)
  (let ((file-contents (fetch-or-create-data full-file-name)))
    (write-complete-file full-file-name (push serialized-data file-contents))
;; (format nil "~&Write ~S~%... to ...~%~A" (push serialized-data file-contents) full-file-name)
    ))

(defun overwrite-data (full-file-name serialized-data)
  (write-complete-file full-file-name serialized-data)
  ;; (format nil "~&Write ~S~%... to ...~%~A~%" serialized-data full-file-name)
  )

(defun get-data-and-file-name (object readers save-name)
  (values (get-full-file-name (class-name (class-of object)) save-name (lambda () (user-id object)))
          (serialize-object->list object readers)))

(defmethod update-object ((object t) readers save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (get-data-and-file-name object readers save-name)
    (prepend-data full-file-name serialized-data)))

(defmethod update-object ((object user-data) readers save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (get-data-and-file-name object readers save-name)
    (let* ((file-contents (fetch-or-create-data full-file-name))
           (data-without-new-row (remove-if
                                 (lambda (e)
                                   (and
                                    (string= (user-id object) (getf e :user-id))
                                    (= (data-id object) (getf e :data-id))))
                                 file-contents)))
      (overwrite-data full-file-name data-without-new-row)
      (prepend-data full-file-name serialized-data))))

(defun overwrite-object (object readers save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (get-data-and-file-name object readers save-name)
    (overwrite-data full-file-name serialized-data)))

(defmethod save-object ((object flat-file) &key readers save-name)
  "Default is to pre-pend; nothing should actually call this."
  (update-object object readers save-name))

(defmethod save-object ((object user-data) &key readers save-name)
  #| TODO - need to make this SMARTER!!
  - if no existing data, then create new
  - if data exists AND it has the same data-id, then update / replace
  - (at some point will have to handle concurrency ...)
  - what's the best way to do this?!? Remove the existing line, then prepend the updated one?
  - if data exists AND it DOES NOT have a match for the same data-id, then pre-pend (how it works now)
  |#
  (update-object object readers save-name))

(defmethod save-object ((object user-settings) &key readers save-name)
  (overwrite-object object readers save-name))

(defmethod save-object ((object user-index) &key readers save-name)
  (update-object object readers save-name))

(defmethod save-object ((object config-data) &key readers save-name)
  (overwrite-object object readers save-name))

(defmethod save-index ((index user-index) &key readers save-name)
  ;; (let ((index-file-name (get-full-file-name (class-name (class-of index)) save-name))))
  (save-object index :save-name save-name :readers readers))

(defun get-full-file-name (class-name save-name &optional get-user-id)
  ;; derive correct file path from object's class name
  (let* ((path-type (get-file-path-type class-name))
         (need-user-id (member path-type (list 'user-settings 'user-data)))
         (directory (apply #'get-file-directory path-type (if need-user-id (list :user-id (funcall get-user-id))))))
    (format nil "~A~A.sexp" directory save-name)))

(defun derive-index-from-where (where)
  (unless (null where)
    (let ((index-name
            (cond
              ((getf where :user-login)
               'user-login-index)
              ((getf where :user-fingerprint)
               'user-fingerprint-index)
              ((getf where :user-apikey)
               'user-apikey-index)
              (t nil))))
      (if index-name
          (string-downcase (symbol-name index-name))
          nil))))

(defun derive-index-from-where-old (where)
  (unless (null where)
    (let* ((key (car where))
           (index-name
             (case key
               (:user-login 'user-login-index)
               (:user-fingerprint 'user-fingerprint-index)
               (:user-apikey 'user-apikey-index)
               (otherwise nil))))
      (if index-name
          (string-downcase (symbol-name index-name))
          nil))))

(defun get-file-path-type (class-name)
  (declare (type symbol class-name))
  (let ((sub-class (find-class class-name)))
    (flet ((subclassp (base-class)
             (subtypep sub-class (find-class base-class))))
      (cond
        ((subclassp 'user-index)
         'user-index)
        ((subclassp 'config-data)
         'configuration)
        ((subclassp 'user-settings)
         'user-settings)
        ((subclassp 'user-data)
         'user-data)))))

(defun get-file-directory (file-path-type &key user-id)
  (case file-path-type
    (user-index (format nil "~A/users/" *app-data-path*))
    (configuration (format nil "~A/" *app-data-path*))
    (user-settings (or
                    (and
                     user-id
                     (format nil "~A/users/~A/" *app-data-path* user-id))
                    (error ":user-id argument required.")))
    (user-data (format nil "~A/users/~A/" *app-data-path* user-id))))

;;; READ seems smart enough to handle lines like you'd want it to
(let ((data (format nil "(:key 'one :name 'first)~C(:key 'two :name 'second)~C(:key 'three :name 'third)~C" #\Linefeed #\Linefeed #\Linefeed)))
  (let ((file-contents (with-input-from-string (input data)
                         (loop for line = (read input nil nil)
                               while line
                               collect
                               (format nil "~S" line)))))
    (format t "~{~A~%~}" file-contents)
    (let ((updated '(:key 'two :name '2nd))
          (output-string (make-array '(0) :element-type 'base-char
                                          :fill-pointer 0 :adjustable t)))
      (with-output-to-string (output output-string)
        (with-input-from-string (input data)
          (loop for line = (read input nil nil)
                while line
                do
                   (if (equal (getf line :key) (getf updated :key))
                       (format output "~S~%" updated)
                       (format output "~S~%" line))))
        output-string))))
