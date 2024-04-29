#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x (string->symbol (format "~a.~a" x (gensym)))))]
      [(Int n) (Int n)]
      [(Let x e body)
       (define new-x (string->symbol (format "~a.~a" x (gensym))))
       (Let new-x
            ((uniquify-exp env) e)
            ((uniquify-exp (cons (cons x new-x) env)) body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : Lvar -> Lvar
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (remove_complex_operands env)
  (letrec ([rco_prim (lambda (e1 e2 op)
                      (define-values (x1 mapping1) (rco_atm e1))
                      (define-values (x2 mapping2) (rco_atm e2))
                      (match (list (or (Int? e1) (Var? e1))
                                    (or (Int? e2) (Var? e2)))
                        ['(#t #t) (Prim op (list e1 e2))]
                        ['(#t #f) (Let x2 (dict-ref mapping2 x2)
                                        (Prim op (list e1 (Var x2))))]
                        ['(#f #t) (Let x1 (dict-ref mapping1 x1)
                                        (Prim op (list (Var x1) e2)))]
                        ['(#t #t) (Let x1 (dict-ref mapping1 x1)
                                        (Let x2 (dict-ref mapping2 x2)
                                            (Prim op (list (Var x1) (Var x2)))))]))]
          [rco_atm (lambda (e)
                      (define tmp-var (string->symbol (format "tmp.~a" (gensym))))
                      (values tmp-var (list (cons tmp-var e))))]
          [rco_exp (lambda (e)
                      (match e
                        [(Int n) (Int n)]
                        [(Var x) (Var x)]
                        [(Prim '- (list _))
                         (define-values (x mapping) (rco_atm e))
                         (Let x (dict-ref mapping x) (Var x))]
                        [(Prim '+ (list e1 e2))
                         (rco_prim e1 e2 '+)]
                        [(Prim '- (list e1 e2))
                         (rco_prim e1 e2 '-)]
                        [(Let x e body)
                         (Let x (rco_exp e) (rco_exp body))]))])
    (lambda (e)
      (rco_exp e))))

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info ((remove_complex_operands '()) e))]))

;; explicate-control : Lvar^mon -> Cvar
(define (explicate-control p)
  (error "TODO: code goes here (explicate-control)"))

;; select-instructions : Cvar -> x86var
(define (select-instructions p)
  (error "TODO: code goes here (select-instructions)"))

;; assign-homes : x86var -> x86var
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : x86var -> x86int
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; prelude-and-conclusion : x86int -> x86int
(define (prelude-and-conclusion p)
  (error "TODO: code goes here (prelude-and-conclusion)"))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(
     ;; Uncomment the following passes as you finish them.
    ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
     ;; ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
     ;; ("instruction selection" ,select-instructions ,interp-x86-0)
     ;; ("assign homes" ,assign-homes ,interp-x86-0)
     ;; ("patch instructions" ,patch-instructions ,interp-x86-0)
     ;; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))
