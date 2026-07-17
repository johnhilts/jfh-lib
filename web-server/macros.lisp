;;;; Web server macros
(cl:in-package #:jfh-web-server)

(defmacro define-api-endpoint (name end-point params &body body)
  `(tbnl:define-easy-handler (,name :uri ,end-point) (,@params)
     "macro to DRY REST endpoint declarations"
     (setf (tbnl:content-type*) "application/json")
     (let* ((raw-data  (tbnl:raw-post-data :force-text t))
            (verb (tbnl:request-method tbnl:*request*))
            (headers (tbnl:headers-in tbnl:*request*)))
       ,@body)))

(defmacro get-form-object (&key is-post accessors post-names get-instance)
  (let ((obj-var (gensym "obj"))
        (get-instance-var (gensym "get-instance")))
    `(if ,is-post
         (list ,@(mapcar (lambda (a p) `(cons ',a (tbnl:post-parameter ,p))) accessors post-names))
         (let ((,get-instance-var ',get-instance))
           (if ,get-instance-var
             (let ((,obj-var ,get-instance))
               (list ,@(mapcar (lambda (a) `(cons ',a (,a ,obj-var))) accessors)))
             nil)))))

(defmacro getv (symbol alist)
  `(cdr (assoc ',symbol ,alist)))
