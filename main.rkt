#lang plai-typed

;; this is a core of the language. We need "desugar" to add more operations
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
)

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
         [else (error 'parse "invalid list input")]))]
    
    [else (error 'parse "invalid input")]
  )
)

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
  )
)



;; here starts desugaring
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
)

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
  )
)


;;(define-type FunDefC
;;  [fdC (name : symbol) (arg : symbol) (body : ExprC)])




;; ===================================================

;; entry point/ pipeline
(define (run [sexp : s-expression]) : number
  (interp ( desugar (parse sexp))))

;; tests
(test (run '(+ 1 2)) 3)                           
(test (run '(* 2 3)) 6)                          
(test (run '(- 5 2)) 3)                           
(test (run '(- (* 2 3) (+ 1 4))) 1)               
(test (run '(+ (* 2 (+ 3 4)) (* 10 6))) 74)
(test (run '(- 7)) -7)
(test (run '(- (* (* 1 1) (+ 3 2)))) -5)