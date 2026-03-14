(defpackage :qiqi
  (:use :cl :iterate))
(in-package :qiqi)

(defclass qiqi-engine (qiku:uci-engine) ()
  (:default-initargs :name "qiqi" :author "Lihui Zhang"))

(defmethod qiku:search-best-move ((engine qiqi-engine) state depth)
  (best-move state depth))

(defun main ()
  (qiku:uci-loop (make-instance 'qiqi-engine)))
