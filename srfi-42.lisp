;;;; srfi-42.lisp

(cl:in-package :srfi-42-internal)

(def-suite srfi-42)

(in-suite srfi-42)

; <PLAINTEXT>
; Eager Comprehensions in [outer..inner|expr]-Convention
; ======================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007
; Scheme R5RS (incl. macros), SRFI-23 (error).
;
; Loading the implementation into Scheme48 0.57:
;   ,open srfi-23
;   ,load ec.scm
;
; Loading the implementation into PLT/DrScheme 317:
;   ; File > Open ... "ec.scm", click Execute
;
; Loading the implementation into SCM 5d7:
;   (require 'macro) (require 'record)
;   (load "ec.scm")
;
; Implementation comments:
;   * All local (not exported) identifiers are named ec-<something>.
;   * This implementation focuses on portability, performance,
;     readability, and simplicity roughly in this order. Design
;     decisions related to performance are taken for Scheme48.
;   * Alternative implementations, Comments and Warnings are
;     mentioned after the definition with a heading.


; ==========================================================================
; The fundamental comprehension do-ec
; ==========================================================================
;
; All eager comprehensions are reduced into do-ec and
; all generators are reduced to :do.
;
; We use the following short names for syntactic variables
;   q    - qualifier
;   cc   - current continuation, thing to call at the end;
;          the CPS is (m (cc ...) arg ...) -> (cc ... expr ...)
;   cmd  - an expression being evaluated for its side-effects
;   expr - an expression
;   gen  - a generator of an eager comprehension
;   ob   - outer binding
;   oc   - outer command
;   lb   - loop binding
;   ne1? - not-end1? (before the payload)
;   ib   - inner binding
;   ic   - inner command
;   ne2? - not-end2? (after the payload)
;   ls   - loop step
;   etc  - more arguments of mixed type


; (do-ec q ... cmd)
;   handles nested, if/not/and/or, begin, :let, and calls generator
;   macros in CPS to transform them into fully decorated :do.
;   The code generation for a :do is delegated to do-ec:do.

(define-syntax do-ec
  (syntax-rules (nested if not and or progn :do let)

    ; explicit nesting -> implicit nesting
    ((do-ec (nested q ***) etc ***)
     (do-ec q *** etc ***) )

    ; implicit nesting -> fold do-ec
    ((do-ec q1 q2 etc1 etc ***)
     (do-ec q1 (do-ec q2 etc1 etc ***)) )

    ; no qualifiers at all -> evaluate cmd once
    ((do-ec cmd)
     (progn cmd (if nil nil)) )

; now (do-ec q cmd) remains

    ; filter -> make conditional
    ((do-ec (if test) cmd)
     (if test (do-ec cmd)) )
    ((do-ec (not test) cmd)
     (if (not test) (do-ec cmd)) )
    ((do-ec (and test ***) cmd)
     (if (and test ***) (do-ec cmd)) )
    ((do-ec (or test ***) cmd)
     (if (or test ***) (do-ec cmd)) )

    ; progn -> make a sequence
    ((do-ec (progn etc ***) cmd)
     (progn etc *** (do-ec cmd)) )

    ; fully decorated :do-generator -> delegate to do-ec:do
    ((do-ec (:do olet lbs ne1? ilet ne2? lss) cmd)
     (|DO-EC:DO| cmd (:do olet lbs ne1? ilet ne2? lss)) )

; anything else -> call generator-macro in CPS; reentry at (*)

    ((do-ec (g arg1 arg ***) cmd)
     (g (|DO-EC:DO| cmd) arg1 arg ***) )))

; (do-ec:do cmd (:do olet lbs ne1? ilet ne2? lss))
;   generates code for a single fully decorated :do-generator
;   with cmd as payload, taking care of special cases.

(define-syntax |DO-EC:DO|
  (syntax-rules (:do let)

    ; reentry point (*) -> generate code
    ((|DO-EC:DO| cmd
               (:do (let obs oc ***)
                    lbs
                    ne1?
                    (let ibs ic ***)
                    ne2?
                    (ls ***) ))
     (ec-simplify
       (let obs
         oc ***
         (sb-int::named-let loop1 lbs
           (ec-simplify
             (if ne1?
                 (ec-simplify
                   (let ibs
                      ic ***
                      cmd
                      (ec-simplify
                        (if ne2?
                            (loop1 ls ***) )))))))))) ))


; (ec-simplify <expression>)
;   generates potentially more efficient code for <expression>.
;   The macro handles if, (progn <command>*), and (let () <command>*)
;   and takes care of special cases.


(define-syntax ec-simplify
  (syntax-rules (if not let progn)

; one- and two-sided if

    ; literal <test>
    ((ec-simplify (if t consequent))
     consequent )
    ((ec-simplify (if nil consequent))
     (if nil nil) )
    ((ec-simplify (if t consequent alternate))
     consequent )
    ((ec-simplify (if nil consequent alternate))
     alternate )

    ; (not (not <test>))
    ((ec-simplify (if (not (not test)) consequent))
     (ec-simplify (if test consequent)) )
    ((ec-simplify (if (not (not test)) consequent alternate))
     (ec-simplify (if test consequent alternate)) )

; (let () <command>*)

    ; empty <binding spec>*
    ((ec-simplify (let () command ***))
     (ec-simplify (progn command ***)) )

; progn

    ; flatten use helper (ec-simplify 1 done to-do)
    ((ec-simplify (progn command ***))
     (ec-simplify 1 () (command ***)) )
    ((ec-simplify 1 done ((progn to-do1 ***) to-do2 ***))
     (ec-simplify 1 done (to-do1 *** to-do2 ***)) )
    ((ec-simplify 1 (done ***) (to-do1 to-do ***))
     (ec-simplify 1 (done *** to-do1) (to-do ***)) )

    ; exit helper
    ((ec-simplify 1 () ())
     (if nil nil) )
    ((ec-simplify 1 (command) ())
     command )
    ((ec-simplify 1 (command1 command ***) ())
     (progn command1 command ***) )

; anything else

    ((ec-simplify expression)
     expression )))


; ==========================================================================
; The special generators :do, :let, :parallel, :while, and :until
; ==========================================================================

(define-syntax :do
  (syntax-rules ()

    ; full decorated -> continue with cc, reentry at (*)
    ((:do (cc ***) olet lbs ne1? ilet ne2? lss)
     (cc *** (:do olet lbs ne1? ilet ne2? lss)) )

    ; short form -> fill in default values
    ((:do cc lbs ne1? lss)
     (:do cc (let ()) lbs ne1? (let ()) t lss) )))

(define-syntax :let
  (syntax-rules (index)
    ((:let cc var (index i) expression)
     (:do cc (let ((var expression) (i 0))) () t (let ()) nil ()) )
    ((:let cc var expression)
     (:do cc (let ((var expression))) () t (let ()) nil ()) )))


(define-syntax :parallel
  (syntax-rules (:do)
    ((:parallel cc)
     cc )
    ((:parallel cc (g arg1 arg ***) gen ***)
     (g (:parallel-1 cc (gen ***)) arg1 arg ***) )))

; (:parallel-1 cc (to-do ***) result [ next ] )
;    iterates over to-do by converting the first generator into
;    the :do-generator next and merging next into result.

(define-syntax :parallel-1  ; used as
  (syntax-rules (:do let)

    ; process next element of to-do, reentry at (**)
    ((:parallel-1 cc ((g arg1 arg ***) gen ***) result)
     (g (:parallel-1 cc (gen ***) result) arg1 arg ***) )

    ; reentry point (**) -> merge next into result
    ((:parallel-1
       cc
       gens
       (:do (let (ob1 ***) oc1 ***)
            (lb1 ***)
            ne1?1
            (let (ib1 ***) ic1 ***)
            ne2?1
            (ls1 ***) )
       (:do (let (ob2 ***) oc2 ***)
            (lb2 ***)
            ne1?2
            (let (ib2 ***) ic2 ***)
            ne2?2
            (ls2 ***) ))
     (:parallel-1
       cc
       gens
       (:do (let (ob1 *** ob2 ***) oc1 *** oc2 ***)
            (lb1 *** lb2 ***)
            (and ne1?1 ne1?2)
            (let (ib1 *** ib2 ***) ic1 *** ic2 ***)
            (and ne2?1 ne2?2)
            (ls1 *** ls2 ***) )))

    ; no more gens -> continue with cc, reentry at (*)
    ((:parallel-1 (cc ***) () result)
     (cc *** result) )))

(define-syntax :while
  (syntax-rules ()
    ((:while cc (g arg1 arg ***) test)
     (g (:while-1 cc test) arg1 arg ***) )))

; (:while-1 cc test (:do ***))
;    modifies the fully decorated :do-generator such that it
;    runs while test is a true value.
;       The original implementation just replaced ne1? by
;    (and ne1? test) as follows:
;
;      (define-syntax :while-1
;        (syntax-rules (:do)
;          ((:while-1 cc test (:do olet lbs ne1? ilet ne2? lss))
;           (:do cc olet lbs (and ne1? test) ilet ne2? lss) )))
;
; Bug #1:
;    Unfortunately, this code is wrong because ne1? may depend
;    in the inner bindings introduced in ilet, but ne1? is evaluated
;    outside of the inner bindings. (Refer to the specification of
;    :do to see the structure.)
;       The problem manifests itself (as sunnan@handgranat.org
;    observed, 25-Apr-2005) when the :list-generator is modified:
;
;      (do-ec (:while (:list x '(1 2)) (= x 1)) (display x)).
;
;    In order to generate proper code, we introduce temporary
;    variables saving the values of the inner bindings. The inner
;    bindings are executed in a new ne1?, which also evaluates ne1?
;    outside the scope of the inner bindings, then the inner commands
;    are executed (possibly changing the variables), and then the
;    values of the inner bindings are saved and (and ne1? test) is
;    returned. In the new ilet, the inner variables are bound and
;    initialized and their values are restored. So we construct:
;
;     (let (ob .. (ib-tmp nil) ***)
;       oc ***
;       (let loop (lb ***)
;         (if (let (ne1?-value ne1?)
;               (let ((ib-var ib-rhs) ***)
;                 ic ***
;                 (setq ib-tmp ib-var) ***)
;               (and ne1?-value test))
;             (let ((ib-var ib-tmp) ***)
;               /payload/
;               (if ne2?
;                   (loop ls ***) )))))
;
; Bug #2:
;    Unfortunately, the above expansion is still incorrect (as Jens-Axel
;    Soegaard pointed out, 4-Jun-2007) because ib-rhs are evaluated even
;    if ne1?-value is nil, indicating that the loop has ended.
;       The problem manifests itself in the following example:
;
;      (do-ec (:while (:list x '(1)) t) (display x))
;
;    Which iterates :list beyond exhausting the list '(1).
;
;    For the fix, we follow Jens-Axel's approach of guarding the evaluation
;    of ib-rhs with a check on ne1?-value.

(define-syntax :while-1
  (syntax-rules (:do let)
    ((:while-1 cc test (:do olet lbs ne1? ilet ne2? lss))
     (:while-2 cc test () () () (:do olet lbs ne1? ilet ne2? lss)))))

(define-syntax :while-2
  (syntax-rules (:do let)
    ((:while-2 cc
               test
               (ib-let     ***)
               (ib-save    ***)
               (ib-restore ***)
               (:do olet
                    lbs
                    ne1?
                    (let ((ib-var ib-rhs) ib ***) ic ***)
                    ne2?
                    lss))
     (:while-2 cc
               test
               (ib-let     *** (ib-tmp nil))
               (ib-save    *** (ib-var ib-rhs))
               (ib-restore *** (ib-var ib-tmp))
               (:do olet
                    lbs
                    ne1?
                    (let (ib ***) ic *** (setq ib-tmp ib-var))
                    ne2?
                    lss)))
    ((:while-2 cc
               test
               (ib-let     ***)
               (ib-save    ***)
               (ib-restore ***)
               (:do (let (ob ***) oc ***) lbs ne1? (let () ic ***) ne2? lss))
     (:do cc
          (let (ob *** ib-let ***) oc ***)
          lbs
          (let ((ne1?-value ne1?))
	    (and ne1?-value
		 (let (ib-save ***)
		   ic ***
		   test)))
          (let (ib-restore ***))
          ne2?
          lss))))


(define-syntax :until
  (syntax-rules ()
    ((:until cc (g arg1 arg ***) test)
     (g (:until-1 cc test) arg1 arg ***) )))

(define-syntax :until-1
  (syntax-rules (:do)
    ((:until-1 cc test (:do olet lbs ne1? ilet ne2? lss))
     (:do cc olet lbs ne1? ilet (and ne2? (not test)) lss) )))


; ==========================================================================
; The typed generators :list :string :vector etc.
; ==========================================================================

(define-syntax :list
  (syntax-rules (index)
    ((:list cc var (index i) arg ***)
     (:parallel cc (:list var arg ***) (:integers i)) )
    ((:list cc var arg1 arg2 arg ***)
     (:list cc var (append arg1 arg2 arg ***)) )
    ((:list cc var arg)
     (:do cc
          (let ())
          ((|t| arg))
          (not (null |t|))
          (let ((var (car |t|))))
          t
          ((cdr |t|)) ))))

(define-syntax :string
  (syntax-rules (index)
    ((:string cc var (index i) arg)
     (:do cc
          (let ((str arg) (len 0))
            (setq len (length str)))
          ((i 0))
          (< i len)
          (let ((var (char str i))))
          t
          ((+ i 1)) ))
    ((:string cc var (index i) arg1 arg2 arg ***)
     (:string cc var (index i) (concatenate 'string arg1 arg2 arg ***)) )
    ((:string cc var arg1 arg ***)
     (:string cc var (index i) arg1 arg ***) )))

; Alternative: An implementation in the style of :vector can also
;   be used for :string. However, it is less interesting as the
;   overhead of string-append is much less than for 'vector-append'.


(define-syntax :vector
  (syntax-rules (index)
    ((:vector cc var arg)
     (:vector cc var (index i) arg) )
    ((:vector cc var (index i) arg)
     (:do cc
          (let ((vec arg) (len 0))
            (setq len (length vec)))
          ((i 0))
          (< i len)
          (let ((var (aref vec i))))
          t
          ((+ i 1)) ))

    ((:vector cc var (index i) arg1 arg2 arg ***)
     (:parallel cc (:vector cc var arg1 arg2 arg ***) (:integers i)) )
    ((:vector cc var arg1 arg2 arg ***)
     (:do cc
          (let ((vec nil)
                (len 0)
                (vecs (|EC-:VECTOR-FILTER| (list arg1 arg2 arg ***))) ))
          ((k 0))
          (if (< k len)
              t
              (if (null vecs)
                  nil
                  (progn (setq vec (car vecs))
                         (setq vecs (cdr vecs))
                         (setq len (length vec))
                         (setq k 0)
                         t )))
          (let ((var (aref vec k))))
          t
          ((+ k 1)) ))))

(defun |EC-:VECTOR-FILTER| (vecs)
  (if (null vecs)
      '()
      (if (zerop (length (car vecs)))
          (|EC-:VECTOR-FILTER| (cdr vecs))
          (cons (car vecs) (|EC-:VECTOR-FILTER| (cdr vecs))) )))

; Alternative: A simpler implementation for :vector uses vector->list
;   append and :list in the multi-argument case. Please refer to the
;   'design.scm' for more details.


(define-syntax :integers
  (syntax-rules (index)
    ((:integers cc var (index i))
     (:do cc ((var 0) (i 0)) t ((+ var 1) (+ i 1))) )
    ((:integers cc var)
     (:do cc ((var 0)) t ((+ var 1))) )))


(define-syntax :range
  (syntax-rules (index)

    ; handle index variable and add optional args
    ((:range cc var (index i) arg1 arg ***)
     (:parallel cc (:range var arg1 arg ***) (:integers i)) )
    ((:range cc var arg1)
     (:range cc var 0 arg1 1) )
    ((:range cc var arg1 arg2)
     (:range cc var arg1 arg2 1) )

; special cases (partially evaluated by hand from general case)

    ((:range cc var 0 arg2 1)
     (:do cc
          (let ((b arg2))
            (if (not (and (integerp b) (rationalp b)))
                (error
                   "arguments of :range are not exact integer ~%(use :real-range?) ~S ~S ~S" 0 b 1 )))
          ((var 0))
          (< var b)
          (let ())
          t
          ((+ var 1)) ))

    ((:range cc var 0 arg2 -1)
     (:do cc
          (let ((b arg2))
            (if (not (and (integerp b) (rationalp b)))
                (error
                   "arguments of :range are not exact integer ~%(use :real-range?) ~S ~S ~S" 0 b 1 )))
          ((var 0))
          (> var b)
          (let ())
          t
          ((- var 1)) ))

    ((:range cc var arg1 arg2 1)
     (:do cc
          (let ((a arg1) (b arg2))
            (if (not (and (integerp a) (rationalp a)
                          (integerp b) (rationalp b) ))
                (error
                   "arguments of :range are not exact integer ~% (use :real-range?) ~S ~S ~S" a b 1 )) )
          ((var a))
          (< var b)
          (let ())
          t
          ((+ var 1)) ))

    ((:range cc var arg1 arg2 -1)
     (:do cc
          (let ((a arg1) (b arg2) (s -1) (stop 0))
            (if (not (and (integerp a) (rationalp a)
                          (integerp b) (rationalp b) ))
                (error
                   "arguments of :range are not exact integer ~%(use :real-range?) ~S ~S ~S" a b -1 )) )
          ((var a))
          (> var b)
          (let ())
          t
          ((- var 1)) ))

; the general case

    ((:range cc var arg1 arg2 arg3)
     (:do cc
          (let ((a arg1) (b arg2) (s arg3) (stop 0))
            (if (not (and (integerp a) (rationalp a)
                          (integerp b) (rationalp b)
                          (integerp s) (rationalp s) ))
                (error
                   "arguments of :range are not exact integer (use :real-range?) ~S ~S ~S" a b s ))
            (if (zerop s)
                (error "step size must not be zero in :range") )
            (setq stop (+ a (* (max 0 (ceiling (/ (- b a) s))) s))) )
          ((var a))
          (not (= var stop))
          (let ())
          t
          ((+ var s)) ))))

; Comment: The macro :range inserts some code to make sure the values
;   are exact integers. This overhead has proven very helpful for
;   saving users from themselves.


(define-syntax :real-range
  (syntax-rules (index)

    ; add optional args and index variable
    ((:real-range cc var arg1)
     (:real-range cc var (index i) 0 arg1 1) )
    ((:real-range cc var (index i) arg1)
     (:real-range cc var (index i) 0 arg1 1) )
    ((:real-range cc var arg1 arg2)
     (:real-range cc var (index i) arg1 arg2 1) )
    ((:real-range cc var (index i) arg1 arg2)
     (:real-range cc var (index i) arg1 arg2 1) )
    ((:real-range cc var arg1 arg2 arg3)
     (:real-range cc var (index i) arg1 arg2 arg3) )

    ; the fully qualified case
    ((:real-range cc var (index i) arg1 arg2 arg3)
     (:do cc
          (let ((a arg1) (b arg2) (s arg3) (istop 0))
            (if (not (and (realp a) (realp b) (realp s)))
                (error "arguments of :real-range are not real ~S ~S ~S" a b s) )
            (if (and (rationalp a) (or (not (rationalp b)) (not (rationalp s))))
                (setq a (float a)) )
            (setq istop (/ (- b a) s)) )
          ((i 0))
          (< i istop)
          (let ((var (+ a (* s i)))))
          t
          ((+ i 1)) ))))

; Comment: The macro :real-range adapts the exactness of the start
;   value in case any of the other values is inexact. This is a
;   precaution to avoid (list-ec (: x 0 3.0) x) => '(0 1.0 2.0).


(define-syntax :char-range
  (syntax-rules (index)
    ((:char-range cc var (index i) arg1 arg2)
     (:parallel cc (:char-range var arg1 arg2) (:integers i)) )
    ((:char-range cc var arg1 arg2)
     (:do cc
          (let ((imax (char-code arg2))))
          ((i (char-code arg1)))
          (<= i imax)
          (let ((var (code-char i))))
          t
          ((+ i 1)) ))))

; Warning: There is no R5RS-way to implement the :char-range generator
;   because the integers obtained by char->integer are not necessarily
;   consecutive. We simply assume this anyhow for illustration.


(define-syntax :port
  (syntax-rules (index)
    ((:port cc var (index i) arg1 arg ***)
     (:parallel cc (:port var arg1 arg ***) (:integers i)) )
    ((:port cc var arg)
     (:port cc var arg read) )
    ((:port cc var arg1 arg2)
     (:do cc
          (let ((port arg1) (read-proc arg2)))
          ((var (funcall read-proc port)))
          (not (eof-object? var))
          (let ())
          t
          ((funcall read-proc port)) ))))


; ==========================================================================
; The typed generator :dispatched and utilities for constructing dispatchers
; ==========================================================================

(define-syntax :dispatched
  (syntax-rules (index)
    ((:dispatched cc var (index i) dispatch arg1 arg ***)
     (:parallel cc
                (:integers i)
                (:dispatched var dispatch arg1 arg ***) ))
    ((:dispatched cc var dispatch arg1 arg ***)
     (:do cc
          (let ((d dispatch)
                (args (list arg1 arg ***))
                (g nil)
                (empty (list nil)) )
            (setq g (funcall d args))
            (if (not (functionp g))
                (error "unrecognized arguments in dispatching ~S ~S" args (funcall d '()) )))
          ((var (funcall g empty)))
          (not (eq var empty))
          (let ())
          t
          ((funcall g empty)) ))))

; Comment: The unique object empty is created as a newly allocated
;   non-empty list. It is compared using eq? which distinguishes
;   the object from any other object, according to R5RS 6.1.

(define-syntax :generator-proc
  (syntax-rules (:do let)

    ; call g with a variable, reentry at (**)
    ((:generator-proc (g arg ***))
     (g (:generator-proc var) var arg ***) )

    ; reentry point (**) -> make the code from a single :do
    ((:generator-proc
       var
       (:do (let obs oc ***)
            ((lv li) ***)
            ne1?
            (let ((i v) ***) ic ***)
            ne2?
            (ls ***)) )
     (ec-simplify
      (let obs
          oc ***
          (let ((lv li) *** (ne2 t))
            (ec-simplify
             (let ((i nil) ***) ; v not yet valid
               (lambda (empty)
                 (if (and ne1? ne2)
                     (ec-simplify
                      (progn
                        (setq i v) ***
                        ic ***
                        (let ((value var))
                          (ec-simplify
                           (if ne2?
                               (ec-simplify
                                (multiple-value-setq (lv ***) (values ls ***))
                                ;; mbeだと 複雑な*** が展開できない?
                                #|(progn (setq lv ls) ***)|# )
                               (setq ne2 nil) ))
                          value )))
                     empty ))))))))

    ; silence warnings of some macro expanders
    ((:generator-proc var)
     (error "illegal macro call") )))


(defun dispatch-union (d1 d2)
  (lambda (args)
    (let ((g1 (funcall d1 args)) (g2 (funcall d2 args)))
      (if g1
          (if g2
              (if (null args)
                  (append (if (listp g1) g1 (list g1))
                          (if (listp g2) g2 (list g2)) )
                  (error "dispatching conflict ~S ~S ~S" args (funcall d1 '()) (funcall d2 '())) )
              g1 )
          (if g2 g2 nil) ))))

; ==========================================================================
; The dispatching generator :
; ==========================================================================

(defun |MAKE-INITIAL-:-DISPATCH| ()
  (lambda (args)
    (case (length args)
      ((0) 'SRFI42)
      ((1) (let ((a1 (car args)))
             (cond
               ((listp a1)
                (print a1)
                (:generator-proc (:list a1)) )
               ((stringp a1)
                (:generator-proc (:string a1)) )
               ((vectorp a1)
                (:generator-proc (:vector a1)) )
               ((and (integerp a1) (rationalp a1))
                (:generator-proc (:range a1)) )
               ((realp a1)
                (:generator-proc (:real-range a1)) )
               #|((streamp a1)
                (:generator-proc (:port a1)) )|#
               (:else
                nil ))))
      ((2) (let ((a1 (car args)) (a2 (cadr args)))
             (cond
               ((and (listp a1) (listp a2))
                (:generator-proc (:list a1 a2)) )
               ((and (stringp a1) (stringp a1))
                (:generator-proc (:string a1 a2)) )
               ((and (vectorp a1) (vectorp a2))
                (:generator-proc (:vector a1 a2)) )
               ((and (integerp a1) (rationalp a1) (integerp a2) (rationalp a2))
                (:generator-proc (:range a1 a2)) )
               ((and (realp a1) (realp a2))
                (:generator-proc (:real-range a1 a2)) )
               ((and (characterp a1) (characterp a2))
                (:generator-proc (:char-range a1 a2)) )
               #|((and (input-portp a1) (functionp a2))
                (:generator-proc (:port a1 a2)) )|#
               (:else
                nil ))))
      ((3) (let ((a1 (car args)) (a2 (cadr args)) (a3 (caddr args)))
             (cond
               ((and (listp a1) (listp a2) (listp a3))
                (:generator-proc (:list a1 a2 a3)) )
               ((and (stringp a1) (stringp a1) (stringp a3))
                (:generator-proc (:string a1 a2 a3)) )
               ((and (vectorp a1) (vectorp a2) (vectorp a3))
                (:generator-proc (:vector a1 a2 a3)) )
               ((and (integerp a1) (rationalp a1)
                     (integerp a2) (rationalp a2)
                     (integerp a3) (rationalp a3))
                (:generator-proc (:range a1 a2 a3)) )
               ((and (realp a1) (realp a2) (realp a3))
                (:generator-proc (:real-range a1 a2 a3)) )
               (:else
                nil ))))
      (otherwise
       (cond
         ((every #'listp args)
          (:generator-proc (:list (apply #'append args))) )
         ((every #'stringp args)
          (:generator-proc (:string (apply #'concatenate 'string args))) )
         ((every #'vectorp args)
          (:generator-proc (:list (apply #'append (mapcar (lambda (x) (coerce x 'list)) args)))) )
         (:else
          nil ))))))

(defvar dispatch-fn
  (|MAKE-INITIAL-:-DISPATCH|) )

(defun :-dispatch-ref ()
  dispatch-fn)

(defun :-dispatch-setq (dispatch)
  (if (not (functionp dispatch))
      (error "not a procedure ~S" dispatch) )
  (setf (symbol-function :-dispatch) dispatch) )

(define-syntax \:
  (syntax-rules (index)
    ((\: cc var (index i) arg1 arg ***)
     (:dispatched cc var (index i) dispatch-fn arg1 arg ***) )
    ((\: cc var arg1 arg ***)
     (:dispatched cc var dispatch-fn arg1 arg ***) )))

(define-syntax :-
  (syntax-rules (index)
    ((:- cc var (index i) arg1 arg ***)
     (:dispatched cc var (index i) dispatch-fn arg1 arg ***) )
    ((:- cc var arg1 arg ***)
     (:dispatched cc var dispatch-fn arg1 arg ***) )))


; ==========================================================================
; The utility comprehensions fold-ec, fold3-ec
; ==========================================================================

(define-syntax fold3-ec
  (syntax-rules (nested)
    ((fold3-ec x0 (nested q1 ***) q etc1 etc2 etc3 etc ***)
     (fold3-ec x0 (nested q1 *** q) etc1 etc2 etc3 etc ***) )
    ((fold3-ec x0 q1 q2 etc1 etc2 etc3 etc ***)
     (fold3-ec x0 (nested q1 q2) etc1 etc2 etc3 etc ***) )
    ((fold3-ec x0 expression f1 f2)
     (fold3-ec x0 (nested) expression f1 f2) )

    ((fold3-ec x0 qualifier expression f1 f2)
     (let ((result nil) (empty t))
       (do-ec qualifier
              (let ((value expression)) ; don't duplicate
                (if empty
                    (progn (setq result (f1 value))
                           (setq empty nil) )
                    (setq result (f2 value result)) )))
       (if empty x0 result) ))))


(define-syntax fold-ec
  (syntax-rules (nested)
    ((fold-ec x0 (nested q1 ***) q etc1 etc2 etc ***)
     (fold-ec x0 (nested q1 *** q) etc1 etc2 etc ***) )
    ((fold-ec x0 q1 q2 etc1 etc2 etc ***)
     (fold-ec x0 (nested q1 q2) etc1 etc2 etc ***) )
    ((fold-ec x0 expression f2)
     (fold-ec x0 (nested) expression f2) )

    ((fold-ec x0 qualifier expression f2)
     (let ((result x0))
       (do-ec qualifier (setq result (f2 expression result)))
       result ))))


; ==========================================================================
; The comprehensions list-ec string-ec vector-ec etc.
; ==========================================================================

(define-syntax list-ec
  (syntax-rules ()
    ((list-ec etc1 etc ***)
     (reverse (fold-ec '() etc1 etc *** cons)) )))

; Alternative: Reverse can safely be replaced by reverse! if you have it.
;
; Alternative: It is possible to construct the result in the correct order
;   using set-cdr! to add at the tail. This removes the overhead of copying
;   at the end, at the cost of more book-keeping.

(defmacro == (x y)
  `(is (equal ,x ,y)))

(test list-ec
  (== (list-ec (\: i 10) i)
      '(0 1 2 3 4 5 6 7 8 9))
  (== (list-ec (\: i 5 10) i)
      '(5 6 7 8 9))
  (== (list-ec (\: i 10) (* i i))
      '(0 1 4 9 16 25 36 49 64 81))
  (== (list-ec (\: i 10) (if (oddp i)) i)
      '(1 3 5 7 9))
  (== (list-ec (\: i 10) (not (evenp i)) i)
      '(1 3 5 7 9))
  (== (list-ec (\: i 10)
               (:let sq (* i i))
               (if (evenp sq))
               sq)
      '(0 4 16 36 64)))

(define-syntax append-ec
  (syntax-rules ()
    ((append-ec etc1 etc ***)
     (apply append (list-ec etc1 etc ***)) )))

(define-syntax string-ec
  (syntax-rules ()
    ((string-ec etc1 etc ***)
     (corece (list-ec etc1 etc ***)
             'string) )))

; Alternative: For very long strings, the intermediate list may be a
;   problem. A more space-aware implementation collect the characters
;   in an intermediate list and when this list becomes too large it is
;   converted into an intermediate string. At the end, the intermediate
;   strings are concatenated with string-append.


(define-syntax string-append-ec
  (syntax-rules ()
    ((string-append-ec etc1 etc ***)
     (apply #'concatenate 'string (list-ec etc1 etc ***)) )))

(define-syntax vector-ec
  (syntax-rules ()
    ((vector-ec etc1 etc ***)
     (coerce (list-ec etc1 etc ***) 'vector) )))

; Comment: A similar approach as for string-ec can be used for vector-ec.
;   However, the space overhead for the intermediate list is much lower
;   than for string-ec and as there is no vector-append, the intermediate
;   vectors must be copied explicitly.

(define-syntax vector-of-length-ec
  (syntax-rules (nested)
    ((vector-of-length-ec k (nested q1 ***) q etc1 etc ***)
     (vector-of-length-ec k (nested q1 *** q) etc1 etc ***) )
    ((vector-of-length-ec k q1 q2             etc1 etc ***)
     (vector-of-length-ec k (nested q1 q2)    etc1 etc ***) )
    ((vector-of-length-ec k expression)
     (vector-of-length-ec k (nested) expression) )

    ((vector-of-length-ec k qualifier expression)
     (let ((len k))
       (let ((vec (make-vector len))
             (i 0) )
         (do-ec qualifier
                (if (< i len)
                    (progn (setf (aref vec  i) expression)
                           (setq i (+ i 1)) )
                    (error "vector is too short for the comprehension") ))
         (if (= i len)
             vec
             (error "vector is too long for the comprehension") ))))))


(define-syntax sum-ec
  (syntax-rules ()
    ((sum-ec etc1 etc ***)
     (fold-ec (+) etc1 etc *** +) )))

(define-syntax product-ec
  (syntax-rules ()
    ((product-ec etc1 etc ***)
     (fold-ec (*) etc1 etc *** *) )))

(define-syntax min-ec
  (syntax-rules ()
    ((min-ec etc1 etc ***)
     (fold3-ec (min) etc1 etc *** min min) )))

(define-syntax max-ec
  (syntax-rules ()
    ((max-ec etc1 etc ***)
     (fold3-ec (max) etc1 etc *** max max) )))

(define-syntax last-ec
  (syntax-rules (nested)
    ((last-ec default (nested q1 ***) q etc1 etc ***)
     (last-ec default (nested q1 *** q) etc1 etc ***) )
    ((last-ec default q1 q2             etc1 etc ***)
     (last-ec default (nested q1 q2)    etc1 etc ***) )
    ((last-ec default expression)
     (last-ec default (nested) expression) )

    ((last-ec default qualifier expression)
     (let ((result default))
       (do-ec qualifier (setq result expression))
       result ))))


; ==========================================================================
; The fundamental early-stopping comprehension first-ec
; ==========================================================================

(define-syntax first-ec
  (syntax-rules (nested)
    ((first-ec default (nested q1 ***) q etc1 etc ***)
     (first-ec default (nested q1 *** q) etc1 etc ***) )
    ((first-ec default q1 q2             etc1 etc ***)
     (first-ec default (nested q1 q2)    etc1 etc ***) )
    ((first-ec default expression)
     (first-ec default (nested) expression) )

    ((first-ec default qualifier expression)
     (let ((result default) (stop nil))
       (ec-guarded-do-ec
         stop
         (nested qualifier)
         (progn (setq result expression)
                (setq stop t) ))
       result ))))

; (ec-guarded-do-ec stop (nested q ***) cmd)
;   constructs (do-ec q *** cmd) where the generators gen in q *** are
;   replaced by (:until gen stop).

(define-syntax ec-guarded-do-ec
  (syntax-rules (nested if not and or progn)

    ((ec-guarded-do-ec stop (nested (nested q1 ***) q2 ***) cmd)
     (ec-guarded-do-ec stop (nested q1 *** q2 ***) cmd) )

    ((ec-guarded-do-ec stop (nested (if test) q ***) cmd)
     (if test (ec-guarded-do-ec stop (nested q ***) cmd)) )
    ((ec-guarded-do-ec stop (nested (not test) q ***) cmd)
     (if (not test) (ec-guarded-do-ec stop (nested q ***) cmd)) )
    ((ec-guarded-do-ec stop (nested (and test ***) q ***) cmd)
     (if (and test ***) (ec-guarded-do-ec stop (nested q ***) cmd)) )
    ((ec-guarded-do-ec stop (nested (or test ***) q ***) cmd)
     (if (or test ***) (ec-guarded-do-ec stop (nested q ***) cmd)) )

    ((ec-guarded-do-ec stop (nested (progn etc ***) q ***) cmd)
     (progn etc *** (ec-guarded-do-ec stop (nested q ***) cmd)) )

    ((ec-guarded-do-ec stop (nested gen q ***) cmd)
     (do-ec
       (:until gen stop)
       (ec-guarded-do-ec stop (nested q ***) cmd) ))

    ((ec-guarded-do-ec stop (nested) cmd)
     (do-ec cmd) )))

; Alternative: Instead of modifying the generator with :until, it is
;   possible to use call-with-current-continuation:
;
;   (define-synatx first-ec
;     ***same as above***
;     ((first-ec default qualifier expression)
;      (call-with-current-continuation
;       (lambda (cc)
;        (do-ec qualifier (cc expression))
;        default ))) ))
;
;   This is much simpler but not necessarily as efficient.


; ==========================================================================
; The early-stopping comprehensions any?-ec every?-ec
; ==========================================================================

(define-syntax any?-ec
  (syntax-rules (nested)
    ((any?-ec (nested q1 ***) q etc1 etc ***)
     (any?-ec (nested q1 *** q) etc1 etc ***) )
    ((any?-ec q1 q2             etc1 etc ***)
     (any?-ec (nested q1 q2)    etc1 etc ***) )
    ((any?-ec expression)
     (any?-ec (nested) expression) )

    ((any?-ec qualifier expression)
     (first-ec nil qualifier (if expression) t) )))

(define-syntax every?-ec
  (syntax-rules (nested)
    ((every?-ec (nested q1 ***) q etc1 etc ***)
     (every?-ec (nested q1 *** q) etc1 etc ***) )
    ((every?-ec q1 q2             etc1 etc ***)
     (every?-ec (nested q1 q2)    etc1 etc ***) )
    ((every?-ec expression)
     (every?-ec (nested) expression) )

    ((every?-ec qualifier expression)
     (first-ec t qualifier (if (not expression)) nil) )))



