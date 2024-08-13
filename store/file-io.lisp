(in-package #:jfh-store)

(defun read-complete-file (path)
  "read complete file all at once"
  (let ((*read-eval* nil))
    (with-open-file (in path :if-does-not-exist :create)
      (with-standard-io-syntax
        (read in nil)))))

(defun write-complete-file (path list)
  "write complete file all at once"
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (prin1 list out)))) ;; print is just like prin1, except it precedes each output with a line break, and ends with a space

(defun fetch-or-create-data (file-path &optional call-back)
  "read data from persistence store; call call back if provided"
  (let ((data (read-complete-file file-path)))
    (if call-back
        (funcall call-back data)
        data)))
