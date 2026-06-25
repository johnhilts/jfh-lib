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

