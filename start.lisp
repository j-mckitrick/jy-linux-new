(defpackage #:jy
  (:use #:cl #:cl-user #:jy-devices #:jy-system)
  (:export
   
   ;; functions
   #:run-experiment
   ))

(in-package #:jy)

;;; create system with some devices
(setf *system* (make-instance '<jy-system>))
;;(configure *system*)
(setf *system* (load-config "hi"))
(init *system*)

;;; add some startops
(dotimes (i 2)
  (with-op (start-op (format nil "scd~A" (1+ i)) :action-set :cmd-int-time '(:exp-value 10))
    (add-start-op (experiment-engine-of *system*) start-op)))

(dotimes (i 2)
  (with-op (start-op (format nil "mono~A" (1+ i)) :action-set :cmd-move '(:exp-value 100))
    (add-start-op (experiment-engine-of *system*) start-op)))

;;; add an axis
(defparameter *axis* (make-instance '<jy-axis>))
(setf (number-of *axis*) 1)
(setf (begin-of *axis*) 1)
(setf (end-of *axis*) 10)
(setf (increment-of *axis*) 1)
(add-axis (experiment-engine-of *system*) *axis*)

(with-op (bound-op "mono2" :action-set :cmd-move '(:exp-value 50))
  (add-bound-op *axis* bound-op))

(with-op (bound-op "scd1" :action-get :cmd-data)
  (add-bound-op *axis* bound-op))

(with-op (bound-op "scd2" :action-get :cmd-data)
  (add-bound-op *axis* bound-op))

;(do-experiment *system*)

;(jy-system::execute-sync (jy-system::experiment-engine-of *system*))

(jy-util:defcp fire-test-event (*test-ready* *test-lock*))

(jy-util:defsink test-sink-fn (*test-ready* *test-lock* :one-time t)
  (jy-util:jy-log "Got test event."))

(defun test-thread ()
  (jy-util:jy-log "Sleeping before sink.")
  (sleep 1.0)
  (jy-util:jy-log "Waiting for event in test sink.")
  (test-sink-fn)
  (jy-util:jy-log "Ending thread."))

(defun test-sync ()
  (sb-thread:make-thread 'test-thread)
  (sleep 2.0)
  (fire-test-event)
  (jy-util:jy-log "Done main thread."))

(defun run-experiment ()
  (execute (experiment-engine-of *system*)))
