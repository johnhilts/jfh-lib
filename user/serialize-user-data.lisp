(in-package #:jfh-user)

(defmethod jfh-store:serialize-object ((application-user application-meta-user) (serialization-type (eql 'application-meta-user)))
  "Input: object and its serialization type."
  (jfh-store:serialize-object->list application-user (list 'user-id 'user-login 'create-date 'disable)))

(defmethod jfh-store:serialize-object ((application-user application-secure-user) (serialization-type (eql 'application-secure-user)))
  "Input: object and its serialization type."
  (jfh-store:serialize-object->list application-user (list 'user-password 'user-fingerprint 'user-api-key)))
