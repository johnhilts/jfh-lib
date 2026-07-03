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
  "Input: a list of objects of type validation-field or a sub-type, and an attribute map for field IDs (optional) . Output: a list of VALIDATION-FAILURE objexts."
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

(defgeneric validate-by-type (type field-maps validation-fields))

(defmethod validate-by-type ((type (eql 'required-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e) (make-instance type :id (car e) :value (cadr e))) validation-fields)))
    (validate-main field-list field-maps)))

(defmethod validate-by-type ((type (eql 'minimum-length-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e)
                              (let ((field (car e))
                                    (value (cadr e))
                                    (minimum (caddr e)))
                                (make-instance type :id field :value value :minimum minimum)))
                            validation-fields)))
    (validate-main field-list field-maps)))

(defmethod validate-by-type ((type (eql 'validation-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e) (make-instance type :id (car e) :value (cadr e))) validation-fields)))
    (validate-main field-list field-maps)))

(defmacro with-validation (mappings validations pass fail)
  (let* ((validation-results (gensym "validation-results"))
         (field-maps-var (gensym "field-maps"))
         (progs
           (loop for param in validations
                 for validation = (car param)
                 for validation-fields = (cadr param)
                 with field-list-var = (gensym "field-list")
                 collect
                 (cond  
                   ((symbol-equal validation 'required)
                    `(validate-by-type 'required-field ,field-maps-var (list ,@(mapcar (lambda (e) `(list ',e ,e)) validation-fields))))
                   ((symbol-equal validation 'minimum-length)
                    `(validate-by-type 'minimum-length-field ,field-maps-var (list ,@(mapcar (lambda (e)
                                                                                               (let ((field (car e))
                                                                                                     (minimum (cadr e)))
                                                                                                 `(list ',field ,field ,minimum)))
                                                                                             validation-fields))))
                   ((symbol-equal validation 'custom)
                    `(let ((,field-list-var (list ,@(mapcar (lambda (e) `(make-instance 'validation-field :id ',e :value ,e)) validation-fields))))
                       (,(caddr param) ,field-list-var ,field-maps-var)))
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
