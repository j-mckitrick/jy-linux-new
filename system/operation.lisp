(in-package :jy-system)

(defclass <jy-operation> ()
  ((device :accessor device-of :initarg device :initform nil)
   (op-type :accessor op-type-of :initarg op-type :initform nil)
   (op-action :accessor op-action-of :initarg op-action :initform nil)
   (op-command :accessor op-command-of :initarg op-command :initform nil)
   (op-parameters :accessor op-parameters-of :initarg op-parameters :initform nil)))

(defmacro with-op ((var device-id op-action op-command &optional op-parameters) &body body)
  `(let ((,var (make-instance '<jy-operation>)))
     (setf (device-of ,var) (get-device-by-id *system* ,device-id)
           (op-action-of ,var) ,op-action
           (op-command-of ,var) ,op-command)
     ,(when op-parameters
            `(setf (op-parameters-of ,var) ,op-parameters))
     ,@body))

(defmacro defit (method-name setter getter)
  `(defmethod ,method-name ((op <jy-operation>))
     (jy-log "~A ~A: ~A params: ~A"
             (op-command-of op) (unique-id-of (device-of op))
             (op-action-of op) (op-parameters-of op))
     (case (op-action-of op)
       ,@(mapcar (lambda (op-action op-fn)
                   `(,op-action (,op-fn (device-of op) (op-parameters-of op))))
                 '(:action-set :action-get) `(,setter ,getter)))))

(defit do-it action-set action-get)
(defit validate-it validate-set validate-get)

