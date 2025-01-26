(in-package #:jfh-store)

(defmethod print-object ((data-store-location data-store-location) stream)
  "Print data store location."
  (print-unreadable-object (data-store-location stream :type t)
    (with-accessors ((settings-file-path settings-file-path)
                     (user-path-root user-path-root))
        data-store-location
      (format stream
	      "Settings File: ~s, User Path: ~s"
	      settings-file-path user-path-root))))

(defmethod initialize-instance :after ((data-store-location data-store-location) &key)
  "Initializations:
- Add ending path separator to any slot with a path for a value"
  (let ((user-path-root #1=(slot-value data-store-location '%user-path-root)))
    (unless (char= #\/ (char user-path-root (1- (length user-path-root))))
      (setf #1# (format nil "~a/" user-path-root)))))

(defmethod get-data-store-location ((data-store-location data-store-location))
  "Input: data store location. *Not* user specific, but the argument can have user-specific information."
  (with-accessors ((settings-file-path settings-file-path)) data-store-location
    settings-file-path))

(defmethod get-user-data-store-location (user-id (data-store-location data-store-location))
  "Input: ID and Data Store Location. Output: file path."
  (with-accessors ((user-path-root user-path-root)) data-store-location
    (format nil "~A~A/" user-path-root user-id)))
;; example: (jfh-store::get-user-data-store-location "abc-123" data-store-location)

(defmethod save-index ((data t) index-name)
  "Input: data and index-name. Index-name will be the main part of the file-name where the index is stored. Output: Not sure yet.")

(defmethod save-data ((data t) name key)
  "Input: data and name; name will be the main part of the file-name where the data is stored. Output: Not sure yet."
  (let ((file-name (format nil "~A~A/~A.sexp" *data-path* key name)))
    (jfh-store:write-complete-file file-name data)))

(defmethod internal/get-data-by-location ((_ file-store-location) (store-object store-object))
  (let ((file-path (format nil "~A/~A.sexp" (location store-object) (label store-object))))
    file-path))

(defmethod internal/get-data-by-location ((_ file-store-location) (store-object user-store-object))
  (let ((file-path (format nil "~A/~A/~A.sexp" (location store-object) (key store-object) (label store-object))))
    file-path))

(defmethod internal/get-data-by-location ((_ file-store-location) (store-object user-index-store-object))
  (let ((file-path (format nil "~A/~A.sexp" (location store-object) (label store-object))))
    (fetch-or-create-data file-path)))

(defmethod internal/save-data-by-location-OLD ((_ file-store-location) (store-object user-store-object) data)
  (let ((file-path (format nil "~A/~A/~A.sexp" (location store-object) (key store-object) (label store-object))))
    (ensure-directories-exist (format nil "~A/~A/" (location store-object) (key store-object)))
    (jfh-store:write-complete-file file-path data)))

(defmethod internal/save-data-by-location ((store-object user-store-object) (store-data store-data))
  (let ((file-path (format nil "~A/~A/~A.sexp" (location store-object) (key store-object) (label store-object))))
    (ensure-directories-exist (format nil "~A/~A/" (location store-object) (key store-object)))
    (jfh-store:write-complete-file file-path (data store-data))))

(defmethod internal/save-data-by-location ((store-object user-index-store-object) (store-data store-data))
  (let ((file-contents (get-data store-object))
        (file-path (format nil "~A/~A.sexp" (location store-object) (label store-object))))
    (jfh-store:write-complete-file file-path (push (data store-data) file-contents))))

(defmethod get-data ((store-object store-object))
  (internal/get-data-by-location store-object store-object))

(defmethod save-user-data-OLD ((store-object store-object) data)
  (internal/save-data-by-location store-object store-object data))

(defmethod save-user-data ((store-data user-store-data))
  (let ((store (make-instance 'jfh-store:user-store-object :label (label store-data) :key (key store-data) :location (format nil "~A/users" *store-root-folder*))))
    (internal/save-data-by-location store store-data)))

(defmethod save-user-data ((store-data user-index-store-data))
  (let ((store (make-instance 'jfh-store:user-index-store-object :label (label store-data) :location (format nil "~A/users" *store-root-folder*))))
    (internal/save-data-by-location store store-data)))

(defmethod serialize-object->list ((object t) accessors) ;; TODO change name to ->PLIST
  "Input: an object and its accessors. Output: plist of accessor values that are serialized to a list. Meant to be used for data with 1 row."
  (loop for accessor in accessors
        nconc
        (list
         (intern (string accessor) (find-package 'keyword))
         (funcall accessor object))))
