#lang plai-typed

;; ===================== CORE LANGUAGE =====================
;; this is a core of the language. We need "desugar" to add more operations
(define-type ExprC
 [numC (n : number)]
 [idC (s : symbol)]
 [appC (fun : symbol) (arg : ExprC)]
 [plusC (l : ExprC) (r : ExprC)]
 [multC (l : ExprC) (r : ExprC)]
)


(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)]
)

;; ======================== PARSER =====================
;; converts an s-expression into the surface language (ArithS).
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond
                [(= (length (rest sl)) 1) (uminusS (parse (second sl)))]
                [else (bminusS (parse (second sl)) (parse (third sl)))]

                )]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    
    [else (error 'parse "invalid input")]
  )
)


;; ===================== INTERPRETER ====================
;; evaluates the core language ArithC into a number.
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    ;;[boolC (b) (if b 1 0)]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    ;;[andC (l r) (if l (if r 1 0) 0)]
    ;;[notC (b) (if b 0 1)]
  )
)

;; subst : ExprC * symbol * ExprC-> ExprC
;; The first argument is what we want to replace the name with
;; The second is for what name we want to perform substitution
;; The third is in which expression we want to do it.

;; its helps us to make a step from formal attributes of the functions
;; to the realy used ones (when function was called)
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]
  )
)

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
 (cond
 [(empty? fds) (error 'get-fundef "reference to undefined function")]
 [(cons? fds) (cond
 [(equal? n (fdC-name (first fds))) (first fds)]
 [else (get-fundef n (rest fds))])]))


;; ================== SURFACE LANGUAGE =================
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [ifS (guard : ArithS) (tCase : ArithS) (fCase : ArithS)]
)

;; ====================== DESUGAR =====================
(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (g t e) (if (zero? (interp (desugar g))) (desugar t) (desugar e))]
  )
)


;; ================= PIPELINE / ENTRY POINT =================
(define (run [sexp : s-expression]) : number
  (interp ( desugar (parse sexp))))


;; ======================== TESTS ===========================
;; tests for basic arithmetic operation
(test (run '(+ 1 2)) 3)                           
(test (run '(* 2 3)) 6)                          
(test (run '(- 5 2)) 3)
(test (run '(- 7)) -7)

;; tests for complicated arithmetic expressions
(test (run '(- (* 2 3) (+ 1 4))) 1)               
(test (run '(+ (* 2 (+ 3 4)) (* 10 6))) 74)
(test (run '(- (* (* 1 1) (+ 3 2)))) -5)

;; tests for conditionals
(test (run '1) 1)
(test (run '(if 0 1 2)) 1)
(test (run '(if 9 1 2)) 2)
(test (run '(if 0 (* 2 3) 2)) 6)
(test (run '(if -1 1 (* 2 3))) 6)
(test (run '(if (- (* 2 3) (+ 1 4)) (- (- 1)) (- (- 2)))) 2)