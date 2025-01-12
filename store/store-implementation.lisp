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

(defmethod save-data ((data t) name)
  "Input: data and name; name will be the main part of the file-name where the data is stored. Output: Not sure yet.")
