#lang plai-typed

(display "Hello to initial commit from PLAI!")
; no displyln here (

(define-type MisspelledAnimal
[caml (humps : number)]
[yacc (height : number)])

(define (good? [ma : MisspelledAnimal]) : boolean
(type-case MisspelledAnimal ma
[caml (humps) (>= humps 2)]
[yacc (height) (> height 2.1)]))


; till here goes actual code, that needed

; this is a core of the language. We need "desugar" to add more operations
(define-type ArithC
[numC (n : number)]
[plusC (l : ArithC) (r : ArithC)]
[multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
(cond
[(s-exp-number? s) (numC (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(case (s-exp->symbol (first sl))
[(+) (plusC (parse (second sl)) (parse (third sl)))]
[(*) (multC (parse (second sl)) (parse (third sl)))]
[else (error 'parse "invalid list input")]))]
[else (error 'parse "invalid input")]))

(define (interp [a : ArithC]) : number
(type-case ArithC a
[numC (n) n]
[plusC (l r) (+ (interp l) (interp r))]
[multC (l r) (* (interp l) (interp r))]))


;; here starts desugaring
(define-type ArithS
[numS (n : number)]
[plusS (l : ArithS) (r : ArithS)]
[bminusS (l : ArithS) (r : ArithS)]
[multS (l : ArithS) (r : ArithS)])

(define (desugar [as : ArithS]) : ArithC
(type-case ArithS as
[numS (n) (numC n)]
[plusS (l r) (plusC (desugar l)
(desugar r))]
[multS (l r) (multC (desugar l)
(desugar r))]
[bminusS (l r) (plusC (desugar l)
(multC (numC -1) (desugar r)))]))
