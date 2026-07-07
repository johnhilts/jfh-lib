(in-package #:jfh-validation)

(defmethod print-object ((validation-field validation-field) stream)
  "Print validation field."
  (print-unreadable-object (validation-field stream :type t)
    (with-accessors ((id id) (value value)) validation-field
      (format stream
	      "ID: ~A, Value: ~S" id value))))

(defmethod print-object ((range-field range-field) stream)
  "Print range field."
  (print-unreadable-object (range-field stream :type t)
    (with-accessors ((id id) (value value) (minimum minimum) (maximum maximum)) range-field
      (format stream
	      "ID: ~A, Value: ~S, Min: ~A, Max: ~A" id value minimum maximum))))

(defmethod print-object ((validation-field-map-value validation-field-map-value) stream)
  "Print validation field map value."
  (print-unreadable-object (validation-field-map-value stream :type t)
    (with-accessors ((text text) (ui-element-id ui-element-id)) validation-field-map-value
      (format stream
	      "TEXT: ~S, UI ELEMENT ID: ~S" text ui-element-id))))

(defmethod print-object ((validation-failure validation-failure) stream)
  "Print validation failure."
  (print-unreadable-object (validation-failure stream :type t)
    (with-accessors ((id id) (message message) (ui-element-id ui-element-id)) validation-failure
      (format stream
	      "ID: ~A, Message: ~S, UI ELEMENT ID: ~S" id message ui-element-id))))

(defmethod is-invalid-p ((validation-field required-field))
  (zerop (length (value validation-field))))

(defmethod build-validation-message ((validation-field required-field) text)
  (format nil "~A is required" text))

(defmethod is-invalid-p ((validation-field minimum-length-field))
  (< (length (value validation-field)) (minimum validation-field)))

(defmethod build-validation-message ((validation-field minimum-length-field) text)
  (format nil "~A requires at least ~D characters." text (minimum validation-field)))

(defmethod validate-by-type ((type (eql 'required-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e) (make-instance type :id (first e) :value (second e))) validation-fields)))
    (validate-main field-list field-maps)))

(defmethod validate-by-type ((type (eql 'minimum-length-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e)
                              (let ((field (first e))
                                    (value (second e))
                                    (minimum (third e)))
                                (make-instance type :id field :value value :minimum minimum)))
                            validation-fields)))
    (validate-main field-list field-maps)))

(defmethod validate-by-type ((type (eql 'validation-field)) field-maps validation-fields)
  (let ((field-list (mapcar (lambda (e) (make-instance type :id (first e) :value (second e))) validation-fields)))
    (validate-main field-list field-maps)))
