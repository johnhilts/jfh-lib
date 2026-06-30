;;;; functions for validation
(cl:in-package #:jfh-validation)

(defun get-field-mapping (field mappings)
  "Input: entry from a field-list of type VALIDATION-FIELD or a derivative.
Output: VALUES of TEXT and UI-ELEMENT-ID."
  (let* ((mapping (cdr (assoc (id field) mappings))))
    (values
     (if mapping (text mapping) (id field))
     (if mapping (ui-element-id mapping) ""))))

(defun validate-main (field-list mappings)
  (loop for field in field-list when (is-invalid-p field)
        collect (multiple-value-bind (text ui-element-id)
                    (get-field-mapping field mappings)
                  (make-instance 'validation-failure :message (build-validation-message field text)
                                                     :id (id field) :ui-element-id ui-element-id))))

(defun map-validation-fields (field text ui-element-id)
  (cons field (make-instance 'validation-field-map-value :text text :ui-element-id ui-element-id)))

(defun symbol-equal (input-symbol compare-symbol)
  (string-equal
   (symbol-name input-symbol)
   (symbol-name compare-symbol)))

(defmacro with-validation (mappings validations pass fail)
  (let* ((validation-results (gensym "validation-results"))
         (field-maps-var (gensym "field-maps"))
         (progs
           (loop for param in validations
                 collect
                 (cond  
                   ((symbol-equal  (car param) 'required)
                    (let ((field-list (gensym "field-list")))
                      `(let ((,field-list (list ,@(mapcar (lambda (e) `(make-instance 'required-field :id ',e :value ,e)) (cadr param)))))
                         (validate-main ,field-list ,field-maps-var))))
                   ((symbol-equal  (car param) 'minimum-length)
                    (let ((field-list (gensym "field-list")))
                      `(let ((,field-list (list ,@(mapcar (lambda (e)
                                                            (let ((field (car e))
                                                                  (minimum (cadr e)))
                                                              `(make-instance 'minimum-length-field :id ',field :value ,field :minimum ,minimum)))
                                                          (cadr param)))))
                         (validate-main ,field-list ,field-maps-var))))
                   ((symbol-equal (car param) 'custom)
                    (let ((field-list (gensym "field-list")))
                      `(let ((,field-list (list ,@(mapcar (lambda (e) `(make-instance 'validation-field :id ',e :value ,e)) (cadr param)))))
                         (,(caddr param) ,field-list ,field-maps-var))))
                   (t (format nil "No matches! (car param) == ~A~%" (car param)))))))
    (let ((pass-block (cdr pass))
          (fail-block (cdr fail)))
      `(let* ((,field-maps-var (mapcar (lambda (e) (map-validation-fields (car e) (cadr e) (caddr e))) ',(cdr mappings)))
              (,validation-results (append ,@progs)))
         (if (not ,validation-results)
             (progn
               ,@pass-block)
             (let ((,(caar fail-block) (reverse
                                        (reduce (lambda (acc cur)
                                                  (pushnew (message cur) acc))
                                                ,validation-results
                                                :initial-value ())))
                   (,(cadar fail-block) (reverse
                                         (reduce (lambda (acc cur)
                                                   (pushnew (ui-element-id cur) acc))
                                                 ,validation-results
                                                 :initial-value ()))))
               ,@(cdr fail-block)))))))
