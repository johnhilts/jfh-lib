(in-package #:jfh-store)

(defmethod print-object ((file-store file-store) stream)
  "Print file store."
  (print-unreadable-object (file-store stream :type t)
    (with-accessors ((path path) (label label)) file-store
      (format stream
	      "Path: ~A, Label: ~A" path label))))

(defmethod initialize-instance :after ((user-data user-data) &key)
  "Initializations:
- Set the ID to a unique ID."
  (let ((id #1=(slot-value user-data '%id)))
    (when (zerop (length id))
      (setf #1# (jfh-utility:generate-unique-token)))))

(defmethod get-data-path ((file-store file-store))
  (format nil "~A/~A.sexp" (path file-store) (label file-store)))

(defmethod get-data-path ((user-data-store user-data-store))
  (format nil "~A/~A/~A.sexp" (path user-data-store) (key user-data-store) (label user-data-store)))

(defmethod get-data-path ((user-index-store user-index-store))
  (format nil "~A/~A.sexp" (path user-index-store) (label user-index-store)))

(defmethod serialize-object->list ((object t) accessors)
  "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used for data with 1 row."
  (loop for accessor in accessors
        nconc
        (list
         (intern (string accessor) (find-package 'keyword))
         (funcall accessor object))))

(defun make-store (class-name &optional key name)
  (let ((sub-class (find-class class-name))
        (save-name (or name class-name)))
    (flet ((subclassp (base-class)
             (subtypep sub-class (find-class base-class))))
      (let ((label (format nil "~(~A~)" save-name)))
        (cond
          ((subclassp 'user-index-data)
           (make-instance 'user-index-store :label label))
          ((subclassp 'user-config-data)
           (make-instance 'user-config-store :label label :key key))
          ((subclassp 'user-data)
           (make-instance 'user-data-store :label label :key key))
          ((subclassp 'data)
           (make-instance 'file-store :label label)))))))

(defun get-data (class-name &key key name)
  (let* ((store (make-store class-name key name))
         (file-path (get-data-path store)))
    (values
     (fetch-or-create-data file-path)
     file-path)))

(defmethod make-instance* ((class-name symbol) &key key (field :user-id) where) ;; TODO write a MAKE-INSTANCE-WHERE so that (FUNCTIONP WHERE); get rid of field, don't use key as a criterion
  (let ((file-contents (get-data class-name :key key)))
    (cond
      ((and where (listp (car file-contents))) ;; only supporting 1 kv pair for now
       (let* ((match-field (car where))
              (match-value (cadr where))
              (equalf (if (stringp match-value) #'string= #'equalp))
              (match (car (remove-if-not (lambda (e) (funcall equalf (getf e match-field) match-value)) file-contents))))
         (if match
             (apply #'make-instance class-name (nconc match (list :user-id key))) ;; TODO pass USER-ID as an optional (or keyword?) parameter when we write MAKE-INSTANCE-WHERE
             nil)))
      ((and key (listp (car file-contents)))
       (let* ((equalf (if (stringp key) #'string= #'equalp))
              (match (car (remove-if-not (lambda (e) (funcall equalf (getf e field) key)) file-contents))))
         (if match
             (apply #'make-instance class-name match)
             nil)))
      (t (apply #'make-instance class-name file-contents)))))

(defun make-instance-list (class-name &key key)
  (let ((file-contents (get-data class-name :key key))) ;; TODO in the future, may replace GET-DATA with something that will READ-LINE instead of do 1 READ
    (loop for line in file-contents
          while line
          collect
          (apply #'make-instance class-name line))))

(defmethod save-object ((user-index-data user-index-data) &key readers &allow-other-keys)
  (let ((class-name (class-name (class-of user-index-data))))
    (multiple-value-bind (file-contents file-path)
        (get-data class-name)
      (let ((serialized-data (serialize-object->list user-index-data readers)))
        (ensure-directories-exist file-path)
        (jfh-store:write-complete-file file-path (push serialized-data file-contents))))))

(defmethod save-object ((user-config-data user-config-data) &key readers key name)
  (let* ((class-name (class-name (class-of user-config-data)))
         (serialized-data (serialize-object->list user-config-data readers))
         (store (make-store class-name key name))
         (file-path (get-data-path store)))
    (ensure-directories-exist file-path)
    (jfh-store:write-complete-file file-path serialized-data)))

;; TODO - read in everything then update if there's a match
(defmethod save-object ((user-data user-data) &key readers key name (save-type :update) &allow-other-keys)
  (let* ((class-name (class-name (class-of user-data))))
    (multiple-value-bind (file-contents file-path)
        (get-data class-name :key key :name name)
      (let ((serialized-data (serialize-object->list user-data readers)))
        (if (eq save-type :new)
            (progn
              (ensure-directories-exist file-path)
              (push serialized-data file-contents))
            (let ((filtered (remove-if (lambda (e) (string-equal (getf serialized-data :id) (getf e :id))) file-contents)))
              (setf file-contents (push serialized-data filtered))))
          (jfh-store:write-complete-file file-path file-contents)))))
;;; save new means that you have to read through the whole file and if not match, add a row
;;; save existing means that you have to read through the whole file and update if there's a match

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
