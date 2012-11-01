;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-


(in-package :pure)

(defmethod node-class ((i <rb-tree>))
  'rb-tree-node)

(defmethod key-interface ((<i> <rb-tree>))
  (order-interface <i>))

(defun node/remove-leftmost (<i> node)
  "Return a tree the same as the one rooted at NODE,
   with the node containing the minimum key removed. See {defun
   tree::node/least}"
  (cond
    ((empty-p <i> node)           (error "remove-least: empty tree"))
    ((empty-p <i> (node/l node))  (node/r node))
    (t
      (node/join <i> (node/k node) (node/v node)
        (node/remove-leftmost <i> (node/l node)) (node/r node)))))


(defun node/remove-rightmost (<i> node)
  "Return a tree the same as the one rooted at NODE,
   with the node containing the maximum key removed. See {defun
   tree::node/greatest}"
  (cond
    ((empty-p <i> node)           (error "remove-greatest: empty tree"))
    ((empty-p <i> (node/r node))  (node/l node))
    (t
      (node/join <i> (node/k node) (node/v node)
        (node/l node) (node/remove-rightmost <i> (node/r node))))))


(defun node/concat2 (<i> node1 node2)
  "Join two trees, the left rooted at NODE1, and the right at NODE2,
   performing a single balancing operation on the resulting tree, if
   needed. Assumes all keys in NODE1 are smaller than all keys in
   NODE2, and the relative balance of NODE1 and NODE2 is such that no
   more than one rotation operation will be required to balance the
   resulting tree"
  (cond
    ((empty-p <i> node1) node2)
    ((empty-p <i> node2) node1)
    (t
      (kv (k v) (leftmost-node <i> node2)
        (node/join <i> k v node1 (node/remove-leftmost <i> node2))))))


(defmethod node/concat  ((<i> <rb-tree>) node1 node2)
  (cond
    ((empty-p <i> node1) node2)
    ((empty-p <i> node2) node1)
    (t
      (let ((n2min (leftmost-node node2)))
        (kv (k v) n2min
          (node/concat3 <i> k v node1  (node/remove-leftmost <i> node2)))))))



(defun node/for-all (<i> node predicate fn)
  "For the side-effect, apply FN to each node of the tree rooted at
  NODE for which the predicate function returns a non-nil value"
  (if (empty-p <i> node) nil
    (kvlr (k v l r) node
      (node/for-all <i> l predicate fn)
      (when (funcall predicate k)
        (funcall (alexandria:ensure-function fn) k v))
      (node/for-all <i> r predicate fn))))




#|

(defvar *rb*) 

(defun rbcheck (&optional (count 144))
  (flet ((put (k v &aux (tree-vector *rb*))          
           (vector-push-extend  
             (pure:insert pure::<rb-tree> (aref tree-vector (1- (length tree-vector))) k v)
             tree-vector))
          (reset ()
            (setf *rb* (make-array 20 :adjustable t :fill-pointer 1 :initial-element nil))))
    (reset)
    (dotimes (i count)
      (put i i))
    (prog1 #1=  (aref *rb* (1- (length *rb*)))
      (check-invariant <rb-tree> #1#)
      (terpri)(describe #1#)(terpri))
    ))

(rbcheck)

#<RB-TREE-NODE (((((((NIL (0 . 0) NIL) (1 . 1) (NIL (2 . 2) NIL)) (3 . 3)..
  [standard-object]

Slots with :INSTANCE allocation:
  KEY     = 63
  VALUE   = 63
  LEFT    = #<RB-TREE-NODE ((((((NIL (0 . 0) NIL) (1 . 1) (NIL (2 . 2) NIL)) (3 . 3)..
  RIGHT   = #<RB-TREE-NODE ((((((NIL (64 . 64) NIL) (65 . 65) (NIL (66 . 66) NIL))..
  HEIGHT  = 9

#<RB-TREE-NODE (((((((NIL (0 . 0) NIL) (1 . 1) (NIL (2 . 2) NIL)) (3 . 3)
                    ((NIL (4 . 4) NIL) (5 . 5) (NIL (6 . 6) NIL)))
                   (7 . 7)
                   (((NIL (8 . 8) NIL) (9 . 9) (NIL (10 . 10) NIL)) (11 . 11)
                    ((NIL (12 . 12) NIL) (13 . 13) (NIL (14 . 14) NIL))))
                  (15 . 15)
                  ((((NIL (16 . 16) NIL) (17 . 17) (NIL (18 . 18) NIL))
                    (19 . 19)
                    ((NIL (20 . 20) NIL) (21 . 21) (NIL (22 . 22) NIL)))
                   (23 . 23)
                   (((NIL (24 . 24) NIL) (25 . 25) (NIL (26 . 26) NIL))
                    (27 . 27)
                    ((NIL (28 . 28) NIL) (29 . 29) (NIL (30 . 30) NIL)))))
                 (31 . 31)
                 (((((NIL (32 . 32) NIL) (33 . 33) (NIL (34 . 34) NIL))
                    (35 . 35)
                    ((NIL (36 . 36) NIL) (37 . 37) (NIL (38 . 38) NIL)))
                   (39 . 39)
                   (((NIL (40 . 40) NIL) (41 . 41) (NIL (42 . 42) NIL))
                    (43 . 43)
                    ((NIL (44 . 44) NIL) (45 . 45) (NIL (46 . 46) NIL))))
                  (47 . 47)
                  ((((NIL (48 . 48) NIL) (49 . 49) (NIL (50 . 50) NIL))
                    (51 . 51)
                    ((NIL (52 . 52) NIL) (53 . 53) (NIL (54 . 54) NIL)))
                   (55 . 55)
                   (((NIL (56 . 56) NIL) (57 . 57) (NIL (58 . 58) NIL))
                    (59 . 59)
                    ((NIL (60 . 60) NIL) (61 . 61) (NIL (62 . 62) NIL))))))
                (63 . 63)
                ((((((NIL (64 . 64) NIL) (65 . 65) (NIL (66 . 66) NIL))
                    (67 . 67)
                    ((NIL (68 . 68) NIL) (69 . 69) (NIL (70 . 70) NIL)))
                   (71 . 71)
                   (((NIL (72 . 72) NIL) (73 . 73) (NIL (74 . 74) NIL))
                    (75 . 75)
                    ((NIL (76 . 76) NIL) (77 . 77) (NIL (78 . 78) NIL))))
                  (79 . 79)
                  ((((NIL (80 . 80) NIL) (81 . 81) (NIL (82 . 82) NIL))
                    (83 . 83)
                    ((NIL (84 . 84) NIL) (85 . 85) (NIL (86 . 86) NIL)))
                   (87 . 87)
                   (((NIL (88 . 88) NIL) (89 . 89) (NIL (90 . 90) NIL))
                    (91 . 91)
                    ((NIL (92 . 92) NIL) (93 . 93) (NIL (94 . 94) NIL)))))
                 (95 . 95)
                 (((((NIL (96 . 96) NIL) (97 . 97) (NIL (98 . 98) NIL))
                    (99 . 99)
                    ((NIL (100 . 100) NIL) (101 . 101) (NIL (102 . 102) NIL)))
                   (103 . 103)
                   (((NIL (104 . 104) NIL) (105 . 105) (NIL (106 . 106) NIL))
                    (107 . 107)
                    ((NIL (108 . 108) NIL) (109 . 109) (NIL (110 . 110) NIL))))
                  (111 . 111)
                  (((((NIL (112 . 112) NIL) (113 . 113) (NIL (114 . 114) NIL))
                     (115 . 115)
                     ((NIL (116 . 116) NIL) (117 . 117) (NIL (118 . 118) NIL)))
                    (119 . 119)
                    (((NIL (120 . 120) NIL) (121 . 121) (NIL (122 . 122) NIL))
                     (123 . 123)
                     ((NIL (124 . 124) NIL) (125 . 125)
                      (NIL (126 . 126) NIL))))
                   (127 . 127)
                   ((((NIL (128 . 128) NIL) (129 . 129) (NIL (130 . 130) NIL))
                     (131 . 131)
                     ((NIL (132 . 132) NIL) (133 . 133) (NIL (134 . 134) NIL)))
                    (135 . 135)
                    (((NIL (136 . 136) NIL) (137 . 137) (NIL (138 . 138) NIL))
                     (139 . 139)
                     ((NIL (140 . 140) NIL) (141 . 141)
                      ((NIL (142 . 142) NIL) (143 . 143) NIL))))))))>
|#
