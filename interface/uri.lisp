;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Universal Resource Identifier

(in-package :interface)

;;;
;;; this is just here to facilitate a learning and prototyping environment of course in
;;; practice there'd be reader conditionalization or some other mechanism
;;;

(let ((pkgs '(:unicly :puri)))
  (unless (notany #'null (mapcar #'find-package pkgs))
    (ql:quickload pkgs)))

;;;
;;; supporting utilities.  Again in production there'd be a bit more care given to the
;;; nuances of uri/urn semantics. I have a decent collection of code implementing such 
;;; things scattered among a couple of other projects that i've used with de.setf.resource
;;; and wilbur.
;;;

(defgeneric render-uri (uri)
  (:method ((uri string))   uri)
  (:method ((uri puri:uri)) (puri:render-uri uri nil))
  (:method ((uri unicly:unique-universal-identifier)) (unicly:uuid-as-urn-string nil uri)))


(defgeneric compare-uri (x y)
  (:method ((x t) (y t))
    (eq x y))
  (:method ((x string) (y string))
    (cond
      ((equalp       x y)  0)
      ((string-lessp x y) -1)
      (t                   1)))
  (:method (x (y puri:uri))
    (- (compare-uri y x)))
  (:method ((x puri:uri) y)
    (compare-uri (render-uri x) (render-uri y)))
  (:method (x (y unicly:unique-universal-identifier))
    (- (compare-uri y x)))
  (:method ((x unicly:unique-universal-identifier) y)
    (compare-uri (render-uri x) (render-uri y)))
  (:method ((x unicly:unique-universal-identifier) (y puri:urn))
    (- (compare-uri y x)))
  (:method ((x puri:urn) (y unicly:unique-universal-identifier))
    (compare-uri (render-uri x) (render-uri y)))
  (:method ((x unicly:unique-universal-identifier) (y puri:uri))
    (compare-uri x (unicly:make-v5-uuid unicly:*uuid-namespace-url* (render-uri y))))
  (:method ((x puri:uri) (y unicly:unique-universal-identifier))
    (- (compare-uri y x)))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier))
    (or (when (unicly:uuid-eql x y) 0)
      (compare-uri (render-uri x) (render-uri y))))
  (:method ((x puri:uri) (y puri:uri))
    (or (when (puri:uri= x y) 0)
      (compare-uri (render-uri x) (render-uri y)))))

;;;
;;; At last, the Interfaces
;;;

(define-interface <uri> (<compare>)
  ((compare-function :initarg :compare :reader compare-function))
  (:parametric (&optional (compare #'compare-uri)) (make-interface :compare compare))
  (:method> uri-p (thing)
    (or (puri:uri-p thing) (unicly:unique-universal-identifier-p thing)))
  (:singleton))


(define-interface <url> (<uri> <makeable>) ()
  (:parametric (&optional (compare #'compare-uri)) (make-interface :compare compare))
  (:singleton))


(defmethod make ((<i> <url>) &key from)
  (etypecase from
    (string   (puri:uri from))
    (puri:uri from)))


(define-interface <urn> (<uri>) ()
  (:parametric (&optional (compare #'compare-uri)) (make-interface :compare compare))
  (:singleton))


(defmethod make ((<i> <urn>) &key identity (ns unicly:*UUID-NAMESPACE-URL*) (type 5))
  (ecase type
    (5 (if identity (unicly:make-v5-uuid ns (render-uri (puri:uri identity))) ns))
    (4 (unicly:make-v4-uuid))
    (0 (unicly:make-null-uuid))))


;;;
;;; testing it out...
;;;


;; (make <url> :from "http://www.gnu.org/")
;; => #<PURI:URI http://www.gnu.org/>
;;
;; (make <url> :from (make <url> :from "http://www.gnu.org/"))
;; => #<PURI:URI http://www.gnu.org/>
;;
;; (compare <url> (make <url> :from "http://www.gnu.org/") (make <url> :from "http://www.gnu.org/"))
;; => 0
;;
;; (== <url> (make <url> :from "http://www.gnu.org/") (make <url> :from "http://www.gnu.org/"))
;; => T
;;
;; (make <urn>)
;; => 6ba7b811-9dad-11d1-80b4-00c04fd430c8
;;
;; (== <urn> (make <urn>) unicly:*uuid-namespace-url*)
;; => T
;;
;; (== <uri> (make <urn>) unicly:*uuid-namespace-url*)
;; => T
;;
;; (make <urn> :type 4)
;; => f8321ec8-6b2e-4bba-99be-5537a4e59f91
;;
;; (== <uri> (make <urn> :type 4) (make <urn> :type 4))
;; => NIL
;;
;; (make <urn> :type 0)
;; => 00000000-0000-0000-0000-000000000000
;;
;; (== <uri> (make <urn> :type 0) (make <urn> :type 0))
;; => T
;;
;; (make <urn> :identity "test")
;; => da5b8893-d6ca-5c1c-9a9c-91f40a2a3649
;;
;; (render-uri (make <urn> :identity "test"))
;; => "urn:uuid:da5b8893-d6ca-5c1c-9a9c-91f40a2a3649"
;;
;; (== <urn> (make <urn> :identity "test") (make <urn> :identity "test"))
;; => T
;;
;; (== <urn> (make <urn> :identity "test") (make <urn> :identity "test0"))
;; => NIL
;;
;; (compare <uri> (make <urn> :identity "test") (make <urn> :identity "test"))
;; => 0
;;
;; (compare <uri> (make <urn> :identity "test") (make <urn> :type 0))
;; => 1
;;
;; (compare <uri>  (make <urn> :type 0) (make <urn> :identity "test"))
;; => -1
;;
;; (compare <uri> (make <url> :from "http://x.com/") (make <urn> :type 0))
;; => 1
;;
;; (compare <uri>  (make <urn> :type 0) (make <url> :from "http://x.com/"))
;; => -1
;;
;; (== <uri> (make <url> :from "http://x.com/") (make <urn> :identity "http://x.com/"))
;; => T
;;
;; (== <uri> (make <url> :from "http://x.com/") (make <urn> :identity "http://x.com"))
;; => T
;;
;; (== <uri> (make <url> :from "http://x.com") (make <urn> :identity "http://x.com/"))
;; => T
;;
;; (== <uri> (make <url> :from "http://y.com/") (make <urn> :identity "http://x.com/"))
;; => NIL
;;

