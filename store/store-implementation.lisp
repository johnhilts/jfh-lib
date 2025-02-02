(in-package #:jfh-store)

(defmethod get-data-path ((file-store file-store))
  (format nil "~A/~A.sexp" (path file-store) (label file-store)))

(defmethod get-data-path ((user-data-store user-data-store))
  (format nil "~A/~A/~A.sexp" (path user-data-store) (key user-data-store) (label user-data-store)))

(defmethod get-data-path ((user-index-store user-index-store))
  (format nil "~A/~A.sexp" (path user-index-store) (label user-index-store)))

(defmethod get-data ((file-store file-store))
  (format nil "Read from: ~A~%"
          (get-data-path file-store)))

(defmethod get-data ((user-data-store user-data-store))
  (format nil "Read User Data from: ~A~%"
          (format nil "~A/~A/~A.sexp" (path user-data-store) (key user-data-store) (label user-data-store))))

(defmethod get-data ((user-config-store user-config-store))
  (format nil "Read User Config Data from: ~A~%"
          (format nil "~A/~A/~A.sexp" (path user-config-store) (key user-config-store) (label user-config-store))))

(defmethod get-data ((user-index-store user-index-store))
  (let ((file-path (get-data-path user-index-store)))
    (fetch-or-create-data file-path)))

(defmethod serialize-object->list ((object t) accessors)
  "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used for data with 1 row."
  (loop for accessor in accessors
        nconc
        (list
         (intern (string accessor) (find-package 'keyword))
         (funcall accessor object))))

(defmethod save-data ((user-index-store user-index-store) (user-index-data user-index-data))
  (let ((file-contents (get-data user-index-store))
        (file-path (get-data-path user-index-store)))
    (jfh-store:write-complete-file file-path (push (serialized-data user-index-data) file-contents))))

(defmethod save-data ((user-config-store user-config-store) (user-config-data user-config-data))
  (let ((file-path (get-data-path user-config-store)))
    (ensure-directories-exist file-path)
    (jfh-store:write-complete-file file-path (serialized-data user-config-data))))
