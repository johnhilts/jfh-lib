(in-package #:jfh-store)

(defparameter *data-store-location* nil)
;; (setf *data-store-location* (make-instance 'data-store-location :settings-file-path "./" :user-path-root "./users/"))

(defun find-and-replace-missing-values (initargs fill-in-values)
  "Input: 1st plist, where some values are '?, 2nd plist which has fill-in values for '? in the 1st plist. Ouptput: the 1st plist modified to replace '? with a fill-in-value."
  (loop for initarg in initargs
        when (and
              (symbolp (getf initargs initarg))
              (string= "?" (symbol-name (getf initargs initarg))))
          do
             (setf (getf initargs initarg) (getf fill-in-values initarg)))
  initargs)

(defun make-instance-from-data-store (class-name initargs id data-store-location &optional (get-data-store-location #'get-user-data-store-location))
  "Input: class and its initarg names+values and an ID. Output: object using make-instance `class-name` with data from parameters + data store."
  (let* ((file-name (format nil "~(~A~).sexp" class-name))
         (file-path (funcall get-data-store-location id data-store-location))
         (entry (read-complete-file (format nil "~A~A" file-path file-name)))
         (initargs-no-missing-values (find-and-replace-missing-values initargs entry)))
    (apply #'make-instance class-name initargs-no-missing-values)))

(defun make-instance-from-data-store-index (index-name class-name initargs data-store-location)
  "Input: class and its initarg names+values and an ID. Output: object using make-instance `class-name` with data from parameters + data store."
  (let* ((file-name (format nil "~(~A~).sexp" index-name))
         (file-path (format nil "~A~A" (user-path-root data-store-location) file-name))
         (complete-index (read-complete-file file-path))
         ;; (initargs-no-missing-values (find-and-replace-missing-values initargs entry))
         (initarg-keys (loop for keys = initargs then (cddr keys)
                             for key = (car keys)
                             while keys
                             collect key))
         (index-entry (find-if (lambda (entry)  (every (lambda (key) (string= (getf entry key) (getf initargs key))) initarg-keys)) complete-index)))
    (if index-entry
        (apply #'make-instance class-name index-entry)
        (format nil "~&File-path: ~A~%complete index: ~A~%keys: ~A" file-path complete-index initarg-keys))))

(defun write-instance-to-data-store (class-name object accessors id data-store-location &optional (get-data-store-location #'get-user-data-store-location))
  "Input: class name, an object and its accessors, and an ID. Output: plist of accessor values that are serialized to the data store. Meant to be used for data with 1 row."
  (let* ((file-name (format nil "~(~A~).sexp" class-name))
         (file-path (funcall get-data-store-location id data-store-location))
         (values (loop for accessor in accessors nconc (list (intern (string accessor) (find-package 'keyword)) (funcall accessor object)))))
    (write-complete-file (format nil "~A~A" file-path file-name) values)))
