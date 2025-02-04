(in-package #:jfh-store)

(defmethod print-object ((file-store file-store) stream)
  "Print file store."
  (print-unreadable-object (file-store stream :type t)
    (with-accessors ((path path) (label label)) file-store
      (format stream
	      "Path: ~A, Label: ~A" path label))))

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

(defun make-store (class-name &optional key)
  (let ((sub-class (find-class class-name)))
    (flet ((subclassp (base-class)
             (subtypep sub-class (find-class base-class))))
      (let ((label (format nil "~(~A~)" class-name)))
        (cond
          ((subclassp 'user-index-data)
           (make-instance 'user-index-store :label label))
          ((subclassp 'user-config-data)
           (make-instance 'user-config-store :label label :key key))
          ((subclassp 'user-data)
           (make-instance 'user-data-store :label label :key key))
          ((subclassp 'data)
           (make-instance 'file-store :label label)))))))

(defun get-data (class-name &key key)
  (let* ((store (make-store class-name key))
         (file-path (get-data-path store)))
    (values
     (fetch-or-create-data file-path)
     file-path)))

(defmethod make-instance* ((class-name symbol) &key key (field :user-id))
  (let ((file-contents (get-data class-name :key key)))
    (cond
      ((and key (listp (car file-contents)))
       (let* ((equalf (if (stringp key) #'string= #'equalp))
              (match (car (remove-if-not (lambda (e) (funcall equalf (getf e field) key)) file-contents))))
         (if match
             (apply #'make-instance class-name match)
             nil)))
      (t (apply #'make-instance class-name file-contents)))))

(defmethod save-object ((user-index-data user-index-data) (readers cons) &key &allow-other-keys)
  (let ((class-name (class-name (class-of user-index-data))))
    (multiple-value-bind (file-contents file-path)
        (get-data class-name)
      (let ((serialized-data (serialize-object->list user-index-data readers)))
        (ensure-directories-exist file-path)
        (jfh-store:write-complete-file file-path (push serialized-data file-contents))))))

(defmethod save-object ((user-config-data user-config-data) (readers cons) &key key)
  (let* ((class-name (class-name (class-of user-config-data)))
         (serialized-data (serialize-object->list user-config-data readers))
         (store (make-store class-name key))
         (file-path (get-data-path store)))
    (ensure-directories-exist file-path)
    (jfh-store:write-complete-file file-path serialized-data)))
