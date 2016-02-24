#lang plai-typed



; Formal Grammar for functions
; S→F(ε)
; F(x)→x a | x a F(x b)
; (ε means empty string)


(define-type ExprC
[numC (n : number)]
[idC (s : symbol)]
[plusC (l : ExprC) (r : ExprC)]
[multC (l : ExprC) (r : ExprC)]
[appC (fun : symbol)(arg : ExprC)]
[ifZeroC (p : ExprC)(t : ExprC)(f : ExprC)])



;; function definition
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(fdC 'double 'x (plusC (idC 'x) (idC 'x)))

(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

(fdC 'const5 '_ (numC 5))


; defining functions in a list
(define functions
  (list
   (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
   (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
   (fdC 'const5 '_ (numC 5))))




;; get-fundef : symbol * (listof FunDefC) -> FunDefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))
 
;  substitutes an expression with another one.
;; subst : ExprC * symbol * ExprC -> ExprC  
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
    [ifZeroC (p t f) (error 'sust "error")]
   

))

;Tests
(test (subst (numC 5) 'x (plusC (idC 'x) (idC 'x)))
      (plusC (numC 5) (numC 5)))

;; parse s-exp -> ExprC
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s)  (idC (s-exp->symbol s))]

  [(and (s-exp-list? s) (= (length (s-exp->list s)) 2)
          (s-exp-symbol? (first (s-exp->list s))))
     (appC
      (s-exp->symbol (first (s-exp->list s)))
      (parse (second (s-exp->list s))))]

    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [(if) (ifZeroC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
         [else (error 'parse "invalid input")]))

"Examples of Parse"
(test (parse '(+ 3 4)) (plusC (numC 3) (numC 4)))
(test (parse '(* 12 7)) (multC (numC 12) (numC 7)))

(test (parse '(+ 23 (+ 23 5)))
      (plusC (numC 23)
            (plusC (numC 23) (numC 5))))
(test (parse (symbol->s-exp 'x)) (idC 'x))

(test (parse '(double 13))
      (appC 'double (numC 13)))
(test(parse '(if 1 2 3))(ifZeroC (numC 1) (numC 2) (numC 3)))
(test (parse (number->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))  


;; interp ExprC -> number
;; evaluate an ExprC expression to numbers.
;; examples
;; (numC 7) -> 7
;; (plusC (numC 3) (numC 4)) -> 7
;; (plusC (ExprC-add (numC 3) (numC 4)) (numC 35)) -> 42  
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
  [numC (n) n]
  [idC (_) (error 'interp "shouldn't get here")]
  [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (subst a
                             (fdC-arg fd)
                             (fdC-body fd))
                      fds))]
  [plusC (l r) (+ (interp l fds) (interp r fds))]
  [multC (l r) (* (interp l fds) (interp r fds))]
  [ifZeroC (p t f) (cond [(<= (interp p fds) 0) (interp t fds)]
    [else (interp f fds)]  )]))



(test (interp (appC 'double (numC 6)) functions) 12)
(test (interp (appC 'quadruple (appC 'const5 (numC 1))) functions) 20)
(test (interp (appC 'double (plusC (numC 3) (multC (numC 4) (numC 5)))) functions) 46)
