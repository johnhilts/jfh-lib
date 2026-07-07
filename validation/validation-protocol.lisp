(in-package #:jfh-validation)

(defclass validation-field ()
  ((%id :accessor id :initarg :id)
   (%value :accessor value :initarg :value)))

(defclass required-field (validation-field)
  ())

(defclass range-field (validation-field)
  ((%minimum :accessor minimum :initarg :minimum)
   (%maximum :accessor maximum :initarg :maximum :initform nil)))

(defclass minimum-length-field (range-field)
  ())

(defclass length-range-field (range-field) ())

(defclass validation-field-map-value ()
  ((%text :accessor text :initarg :text)
   (%ui-element-id :accessor ui-element-id :initarg :ui-element-id)))

(defclass validation-failure ()
  ((%id :accessor id :initarg :id)
   (%message :accessor message :initarg :message)
   (%ui-element-id :accessor ui-element-id :initarg :ui-element-id)))

(defgeneric is-invalid-p (validation-field))

(defgeneric build-validation-message (validation-field text))

(defgeneric validate-by-type (type field-maps validation-fields))
