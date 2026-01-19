;;;; TODO
;;;; NOTE: check if a slot is bound:
;;; (let ((object (make-instance 'jfh-user::application-user :user-login "me@here.com" :user-id "abc-123")))
;;; (cl:slot-boundp object (closer-mop:slot-definition-name (car (closer-mop:class-direct-slots (find-class (class-name (class-of object))))))))
;;;; Need to create an object to store the reader name and a lambda exp to check slot-boundp
;;;; GET-EFFECTIVE-READERS is only used in 1 method
(in-package #:jfh-store)

(defparameter *effective-readers* (make-hash-table))

;; TODO - need to update
;; (defmethod print-object ((file-store file-store) stream)
;;   "Print file store."
;;   (print-unreadable-object (file-store stream :type t)
;;     (with-accessors ((path path) (label label)) file-store
;;       (format stream
;; 	      "Path: ~A, Label: ~A" path label))))

(defmethod initialize-instance :after ((user-data user-data) &key)
  "Initializations:
- Set the ID to a unique ID."
  (let ((data-id #1=(slot-value user-data '%data-id)))
    (when (zerop (length data-id))
      (setf #1# (jfh-utility:generate-unique-token)))))

(defmethod print-object ((reader-entry reader-entry) stream)
  "Print reader entry."
  (print-unreadable-object (reader-entry stream :type t)
    (with-accessors ((reader-name reader-name)) reader-entry
      (format stream
	      "Reader: \"~A\"" reader-name))))

(defun get-effective-readers-r (class &optional (accumulator nil))
  "The recursive part"
  (labels ((exclude-field-p (field)
             (if *non-serialized-fields*
                 (find (car (closer-mop:slot-definition-readers field)) *non-serialized-fields* :test #'eql)
                 nil)))
    (flet ((standard-super-class-p (super-class)
             (or (eql (find-class 'standard-object) super-class)
                 (eql (find-class 'standard-class) super-class)))
           (coalesce-to-list (item) ;; TODO I think this is in onlisp or LoL ... get that name
             (if (listp item)
                 item
                 (list item)))
           (make-reader-entry (slot)
             (make-instance 'reader-entry
                            :reader-name (car (closer-mop:slot-definition-readers slot)) ;; assuming only 1 reader per slot
                            :slot-boundp-check (lambda (object) (slot-boundp object (closer-mop:slot-definition-name slot)))))
           (unique-readers (accumulator direct-readers)
             (remove-duplicates (append accumulator direct-readers) :test (lambda (e1 e2) (string= (symbol-name e1) (symbol-name e2))) :key #'reader-name))
           (get-direct-slots (class)
             (let* ((direct-slots (closer-mop:class-direct-slots class)))
               (if *non-serialized-fields*
                   (remove-if #'exclude-field-p direct-slots)
                   direct-slots))))
      (let ((direct-readers (mapcar #'make-reader-entry (get-direct-slots class)))
            (direct-superclasses (remove-if #'standard-super-class-p (closer-mop:class-direct-superclasses class))))
        (if direct-superclasses
            (reduce (lambda (acc cur) (get-effective-readers-r cur acc)) direct-superclasses :initial-value (unique-readers accumulator direct-readers))
            (unique-readers (coalesce-to-list accumulator) direct-readers))))))

(defun get-effective-readers (class)
  "Does the actual work to get the readers for a class. Assumes CLASS inherits JFH-STORE:FLAT-FILE."
  (let ((cached-readers (gethash class *effective-readers*)))
    (if cached-readers
        cached-readers
        (setf (gethash class *effective-readers*) (get-effective-readers-r class)))))

(defun choose-where-comparer (value)
  (typecase value
    (vector #'equalp)
    (number #'=)
    (otherwise #'string=)))

(defun is-index-p (class-name)
  (subtypep (find-class class-name) (find-class 'jfh-store:user-index)))

(defun get-file-contents (user-id class-name where &key fetch-multiple)
  (let* ((need-index-lookup (and (null user-id) where (not (is-index-p class-name))))
         (index-full-file-name (and need-index-lookup (get-index-full-file-name where)))
         (key-user-id (or user-id (and need-index-lookup (getf (read-from-index index-full-file-name where) :user-id))))
         (full-file-name (get-full-file-name class-name (string-downcase (string class-name)) (lambda () key-user-id)))
         (file-contents (fetch-or-create-data full-file-name)))
    (if (and where (consp (car file-contents)))
        (destructuring-bind (key value)
            ;; TODO - make WHERE handle multiple records
            where ;; NOTE - assuming only 1 "clause"; TODO - support any number of clauses, or a lambda (easier!)
          (let ((comparer (choose-where-comparer value)))
            (interpolate-user-id-into key-user-id
                                      (car
                                       (remove-if-not
                                        (lambda (e)
                                          (funcall comparer value (getf e key))) ;; TODO - be more robust than STRING= and EQUALP
                                        file-contents)))))
        (if (consp (car file-contents))
            (if fetch-multiple
                (loop for row in file-contents
                      collect
                      (interpolate-user-id-into key-user-id row))
                (progn
                  (warn "Multiple rows returned; retrieving first one; for multiple rows use MAKE-INSTANCE-LIST instead.")
                  (interpolate-user-id-into key-user-id (car file-contents))))
            (interpolate-user-id-into key-user-id file-contents)))))

(defun make-instance-list (class-name &key where user-id)
  (let ((file-contents (get-file-contents user-id class-name where :fetch-multiple t)))  ;; TODO - SIGNAL jfh-store:no-data-match if NIL
    (loop for row in file-contents
          collect
          (apply #'make-instance class-name row))))

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
      (let ((comparer (if (stringp value) #'string= #'equal)))
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
  ;; TODO - (ERROR) if trying to apply MAKE-INSTANCE* to an index type?
  (let ((file-contents (get-file-contents user-id class-name where)))  ;; TODO - SIGNAL jfh-store:no-data-match if NIL
    (unless (null file-contents)
      (apply #'make-instance class-name file-contents))))

(defmethod serialize-object->list ((object user-settings))
  "Input: an object. Output: plist of accessor values that are serialized to a list. Meant to be used with 1 \"row\" of data."
  (let ((*non-serialized-fields* '(user-id)))
    (when (next-method-p)
      (call-next-method))))

(defmethod serialize-object->list ((object user-data))
  "Input: an object. Output: plist of accessor values that are serialized to a list. Meant to be used with 1 \"row\" of data."
  (let ((*non-serialized-fields* '(user-id)))
    (when (next-method-p)
      (call-next-method))))

(defmethod serialize-object->list ((object flat-file))
  "Input: an object. Output: plist of accessor values that are serialized to a list. Meant to be used with 1 \"row\" of data."
  (let* ((reader-entries (get-effective-readers (find-class (class-name (class-of object))))))
    (loop for reader-entry in reader-entries ;; TODO add filter here using specialization to remove the readers we don't want
          for reader = (reader-name reader-entry)
          when (funcall (slot-boundp-check reader-entry) object) ;; this part has to reach back into the HT
            nconc
            (list
             (intern (string reader) (find-package 'keyword))
             (funcall reader object)))))

(defun prepend-data (full-file-name serialized-data)
  (let ((file-contents (fetch-or-create-data full-file-name)))
    (write-complete-file full-file-name (push serialized-data file-contents))))

(defun overwrite-data (full-file-name serialized-data)
  (write-complete-file full-file-name serialized-data))

(defun serialize-data-and-get-file-name (object save-name)
  (values (get-full-file-name (class-name (class-of object)) save-name (lambda () (user-id object)))
          (serialize-object->list object)))

(defmethod update-object ((object t) save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (serialize-data-and-get-file-name object save-name)
    (prepend-data full-file-name serialized-data)))

(defmethod update-object ((object user-index) save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (serialize-data-and-get-file-name object save-name)
    (let* ((file-contents (fetch-or-create-data full-file-name))
           (data-without-new-row (remove-if
                                  (lambda (e)
                                    (string= (user-id object) (getf e :user-id)))
                                  file-contents)))
      (overwrite-data full-file-name data-without-new-row)
      (prepend-data full-file-name serialized-data))))

(defmethod update-object ((object user-data) save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (serialize-data-and-get-file-name object save-name)
    (let* ((file-contents (fetch-or-create-data full-file-name))
           (data-without-new-row (remove-if
                                  (lambda (e)
                                    (string= (data-id object) (getf e :data-id)))
                                  file-contents)))
      (overwrite-data full-file-name data-without-new-row)
      (prepend-data full-file-name serialized-data))))

(defun overwrite-object (object save-name)
  (multiple-value-bind
        (full-file-name serialized-data)
      (serialize-data-and-get-file-name object save-name)
    (overwrite-data full-file-name serialized-data)))

(defmethod delete-data ((object user-data) save-name)
  (let* ((full-file-name (get-full-file-name (class-name (class-of object)) save-name (lambda () (user-id object))))
         (file-contents (fetch-or-create-data full-file-name))
         (data-without-deleted-row (remove-if
                                    (lambda (e)
                                      (string= (data-id object) (getf e :data-id)))
                                    file-contents)))
    (overwrite-data full-file-name data-without-deleted-row)))

(defun get-save-name (object)
  "Get the default save name for an object."
  (string-downcase (string (class-name (class-of object)))))

(defmethod save-object ((object flat-file) &key save-name)
  "Default is to pre-pend; nothing should actually call this."
  (update-object object (or save-name (get-save-name object))))

(defmethod save-object ((object user-data) &key save-name)
  #| TODO - need to make this SMARTER!!
  - if no existing data, then create new
  - if data exists AND it has the same data-id, then update / replace
  - (at some point will have to handle concurrency ...)
  - what's the best way to do this?!? Remove the existing line, then prepend the updated one?
  - if data exists AND it DOES NOT have a match for the same data-id, then pre-pend (how it works now)
  |#
  (update-object object (or save-name (get-save-name object))))

(defmethod save-object ((object user-settings) &key save-name)
  (overwrite-object object (or save-name (get-save-name object))))

(defmethod save-object ((object user-index) &key save-name)
  (update-object object (or save-name (get-save-name object))))

(defmethod save-object ((object config-data) &key save-name)
  (overwrite-object object (or save-name (get-save-name object))))

(defmethod save-index ((index user-index) &key save-name)
  ;; (let ((index-file-name (get-full-file-name (class-name (class-of index)) save-name))))
  (save-object index :save-name (or save-name (get-save-name index))))

(defmethod delete-object ((object user-data) &key save-name)
  (delete-data object (or save-name (get-save-name object))))

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
              ((getf where :user-api-key)
               'user-api-key-index-entry)
              (t nil))))
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
#+dont-run-this
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
