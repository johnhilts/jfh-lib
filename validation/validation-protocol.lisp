(in-package #:jfh-validation)

(defclass validation-field ()
  ((%id :accessor id :initarg :id)
   (%value :accessor value :initarg :value)))

(defclass range-field (validation-field)
  ((%minimum :accessor minimum :initarg :minimum)
   (%maximum :accessor maximum :initarg :maximum :initform nil)))

(defclass length-range-field (range-field) ())

(defclass validation-field-map-value ()
  ((%text :accessor text :initarg :text)
   (%ui-element-id :accessor ui-element-id :initarg :ui-element-id)))

(defclass validation-failure ()
  ((%id :accessor id :initarg :id)
   (%message :accessor message :initarg :message)
   (%ui-element-id :accessor ui-element-id :initarg :ui-element-id)))

