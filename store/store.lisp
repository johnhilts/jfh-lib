(in-package #:jfh-store)

(defun find-and-replace-missing-values (initargs fill-in-values)
  "Input: 1st plist, where some values are '?, 2nd plist which has fill-in values for '? in the 1st plist. Ouptput: the 1st plist modified to replace '? with a fill-in-value."
  (loop for initarg in initargs
        when (and
              (symbolp (getf initargs initarg))
              (string= "?" (symbol-name (getf initargs initarg))))
          do
             (setf (getf initargs initarg) (getf fill-in-values initarg)))
  initargs)

(defun make-instance-with-partial-data (class-name initargs)
  "Input: class and its initarg names+values. Output: object using make-instance `class-name` with data from parameters + data store."
  (let* ((file-contents (jfh-store:get-data class-name))
         (initargs-no-missing-values (find-and-replace-missing-values initargs file-contents)))
    (apply #'make-instance class-name initargs-no-missing-values)))
