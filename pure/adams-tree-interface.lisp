;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-


(in-package :pure)

;;; pure RB-TREE

(define-interface <rb-tree> (interface::<rb-tree> <binary-tree> <order-parameter>) ()
  (:parametric (&optional (order interface::<universal-order>))
    (make-interface :order order))
  (:singleton))


(defclass rb-tree-node (interface::rb-tree-node binary-tree-node)
  ())
