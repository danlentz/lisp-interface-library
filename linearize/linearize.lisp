;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful and back

#+xcvb (module (:depends-on ("interface/interface" "interface/box")))

(in-package :interface)

;;; TODO: handle gf's with or without explicit override

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun un<>ate (string)
    (let ((string (string string)))
      (if (string-enclosed-p "<" string ">")
          (subseq string 1 (1- (length string)))
          string))))

(defmacro define-linearized-method
    (linearized-interface pure-classes stateful-classes
     pure-gf stateful-gf &key
     pure-lambda-list pure-values pure-effects pure-gf-options
     stateful-lambda-list stateful-values stateful-effects stateful-gf-options)
  (nest
   (progn
     (dolist (class (append pure-classes stateful-classes))
       (finalize-inheritance class)))
   (let* ((pure-gf-options
           (or pure-gf-options
               (interface-gf-options pure-classes pure-gf)))
          (stateful-gf-options
           (or stateful-gf-options
               (interface-gf-options stateful-classes stateful-gf)))
          (pure-gf* (symbol-function pure-gf))
          (stateful-gf* (symbol-function stateful-gf))
          (pure-lambda-list
           (or pure-lambda-list
               (getf pure-gf-options :lambda-list)
               (closer-mop:generic-function-lambda-list pure-gf*)))
          (stateful-lambda-list
           (or stateful-lambda-list
               (getf stateful-gf-options :lambda-list)
               (closer-mop:generic-function-lambda-list stateful-gf*)))
          (pure-values
           (or pure-values
               (getf pure-gf-options :values)))
          (stateful-values
           (or stateful-values
               (getf stateful-gf-options :values)))
          (pure-effects
           (or pure-effects
               (getf pure-gf-options :effects)))
          (stateful-effects
           (or stateful-effects
               (getf stateful-gf-options :effects)))))
   (multiple-value-bind (pure-mimic-lambda-list
                         pure-mimic-ignorables
                         pure-mimic-invoker pure-mimic-arguments
                         pure-mimic-mappings)
       (lambda-list-mimicker pure-lambda-list)
     (declare (ignore pure-mimic-invoker pure-mimic-arguments pure-mimic-mappings)))
   (multiple-value-bind (stateful-mimic-lambda-list
                         stateful-mimic-ignorables
                         stateful-mimic-invoker stateful-mimic-arguments
                         stateful-mimic-mappings)
       (lambda-list-mimicker stateful-lambda-list t)
     (declare (ignore stateful-mimic-ignorables stateful-mimic-mappings)))
   (multiple-value-bind (pure-required pure-optionals
                         pure-rest pure-keys pure-allow-other-keys pure-aux)
         (alexandria:parse-ordinary-lambda-list pure-lambda-list)
     (declare (ignore pure-keys pure-allow-other-keys pure-aux)))
   (multiple-value-bind (stateful-required stateful-optionals
                         stateful-rest stateful-keys stateful-allow-other-keys stateful-aux)
         (alexandria:parse-ordinary-lambda-list stateful-mimic-lambda-list)
     (declare (ignore stateful-keys stateful-allow-other-keys stateful-aux)))
   (multiple-value-bind (pure-results-lambda-list
                         pure-results-ignorables
                         pure-results-invoker pure-results-arguments
                         pure-results-mappings)
       (lambda-list-mimicker pure-values t)
     (declare (ignore pure-results-ignorables pure-results-mappings)))
   (multiple-value-bind (stateful-results-lambda-list
                         stateful-results-ignorables
                         stateful-results-invoker stateful-results-arguments
                         stateful-results-mappings)
       (lambda-list-mimicker stateful-values t)
     (declare (ignore stateful-results-invoker stateful-results-arguments
                      stateful-results-mappings)))
   (multiple-value-bind (pure-results-required pure-results-optionals
                         pure-results-rest pure-results-keys
                         pure-results-allow-other-keys pure-results-aux)
         (alexandria:parse-ordinary-lambda-list pure-results-lambda-list)
     (declare (ignore pure-results-keys pure-results-allow-other-keys
                      pure-results-aux)))
   (multiple-value-bind (stateful-results-required stateful-results-optionals
                         stateful-results-rest stateful-results-keys
                         stateful-results-allow-other-keys stateful-results-aux)
         (alexandria:parse-ordinary-lambda-list stateful-results-lambda-list)
     (declare (ignore stateful-results-keys stateful-results-allow-other-keys
                      stateful-results-aux)))
   (let ((pi-var (first pure-required))
         (si-var (first stateful-required))
         (lpin (length pure-required))
         (lsin (length stateful-required))
         (lpout (length pure-results-required))
         (lsout (length stateful-results-required)))
     (assert (plusp lpin))
     (assert (= lpin lsin))
     (assert (= (length pure-optionals) (length stateful-optionals)))
     (assert (eq (and pure-rest t) (and stateful-rest t))))
   (loop
     :with lepout = 0 :with lesout = 0
     :for (pin pout) :in pure-effects
     :for (sin sout) :in stateful-effects
     :do (assert (eq (integerp pin) (integerp sin)))
         (assert (eq (null pout) (null sout))) ;; new is new
         (check-type pout (or integer null)) ;; pure is pure
     :when (integerp pin)
       :collect (list pin pout sin sout) :into effective-inputs :end
     :when (integerp pout)
       :collect (list pout pin sout sin) :into effective-outputs :end
     :when (integerp pout)
       :do (incf lepout) :end
     :when (integerp sout)
       :do (incf lesout) :end
     :finally
     (assert (= (- lpout lepout) (- lsout lesout))))
   (return)
   (let* ((required-input-bindings
           (loop :for (epi epo esi ()) :in effective-inputs
             :for piv = (nth epi pure-required)
             :for siv = (nth esi stateful-required)
             :collect `(,siv (,(if epo 'box-ref 'box-value) ,piv))))
          (required-output-bindings
           (loop :for (epo () eso esi) :in effective-outputs
             :for pov = (nth epo pure-results-required)
             :collect `(,pov
                        (make-instance 'one-use-value-box :value
                         ,(etypecase eso
                            (integer (nth eso stateful-results-required))
                            ((eql t) (nth esi stateful-required)))))))
          (ineffective-pure-inputs
           (loop :for i :from 1 :below lpin
             :for v :in pure-required
             :unless (find i effective-inputs :key 'first)
             :collect v))
          (ineffective-stateful-inputs
           (loop :for i :from 1 :below lsin
             :for v :in stateful-required
             :unless (find i effective-inputs :key 'third)
             :collect v))
          (ineffective-pure-outputs
           (loop :for i :below lpout
             :for v :in pure-results-required
             :unless (find i effective-outputs :key 'first)
             :collect v))
          (ineffective-stateful-outputs
           (loop :for i :below lpout
             :for v :in stateful-results-required
             :unless (find i effective-outputs :key 'third)
             :collect v))
          (stateful-argument-bindings
           (append
            `((,si-var (stateful-interface ,pi-var)))
            required-input-bindings
            (loop :for ipi :in ineffective-pure-inputs
              :for isi :in ineffective-stateful-inputs
              :collect `(,isi ,ipi))
            (loop :for (po () pop) :in pure-optionals
              :for (so () sop) :in stateful-optionals
              :append `((,so ,po) (,pop ,sop)))
            (when stateful-rest
              `((,stateful-rest ,pure-rest)))))
          (pure-results-bindings
           (append
            required-output-bindings
            (loop :for ipo :in ineffective-pure-outputs
              :for iso :in ineffective-stateful-outputs
              :collect `(,ipo ,iso))
            (loop :for (pro () prop) :in pure-results-optionals
              :for (sro () srop) :in stateful-results-optionals
              :append `((,pro ,sro) (,prop ,srop)))
            (when pure-results-rest
              `((,pure-results-rest ,stateful-results-rest)))))))
  `(defmethod ,pure-gf ((,pi-var ,linearized-interface) ,@(rest pure-mimic-lambda-list))
     (declare (ignore ,@pure-mimic-ignorables))
     (let* (,@stateful-argument-bindings)
       (multiple-value-bind (,@stateful-results-lambda-list)
           (,stateful-mimic-invoker ',stateful-gf ,si-var ,@(rest stateful-mimic-arguments))
         (declare (ignore ,@stateful-results-ignorables))
         (let* (,@pure-results-bindings)
           (,pure-results-invoker #'values ,@pure-results-arguments)))))))

(defmacro define-linearized-interface
    (name pure-classes stateful-classes &rest options)
    (let* ((all-pure-classes (all-superclasses pure-classes))
           (pure-gfs (all-interface-generics all-pure-classes))
           (all-stateful-classes (all-superclasses stateful-classes))
           (stateful-gfs (all-interface-generics all-stateful-classes))
           (stateful-gfs-hash
            (alexandria:alist-hash-table
             (mapcar (lambda (x) (cons (symbol-name x) x)) stateful-gfs) :test 'equal))
           (overridden-gfs (find-multiple-clos-options :method options))
           (overridden-gfs-hash
            (alexandria:alist-hash-table
             (mapcar (lambda (x) (cons (second x) (nthcdr 2 x))) overridden-gfs) :test 'eql)))
      `(progn
         (define-interface ,name (<linearized> ,@pure-classes)
           ()
           ,@options)
         ,@(loop :for pure-gf :in pure-gfs
             :unless (gethash pure-gf overridden-gfs-hash) :append
             (nest
              (let ((pure-effects (getf (search-gf-options all-pure-classes pure-gf) :effects))))
               ;; methods that have registered effects as expressible and expressed in our trivial language
              (when pure-effects)
              (let* ((stateful-gf (gethash (symbol-name pure-gf) stateful-gfs-hash))
                     (stateful-effects (getf (search-gf-options all-stateful-classes stateful-gf) :effects)))
                (assert stateful-effects))
              `((define-linearized-method ,name ,pure-classes ,stateful-classes
                                          ,pure-gf ,stateful-gf)))))))

(in-package :pure)

(define-interface <linearized> ()
  ((stateful-interface
    :reader stateful-interface
    :initarg :stateful-interface)
   #|(box-interface
    :reader box-interface
    :initarg :box-interface :initform <one-use-value-box>)|#)
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
