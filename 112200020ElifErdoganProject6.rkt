#lang plai-typed
;Formal Grammar

; Alphabet {+,-,*,**,id, {,}}
; T -> Terminal symbols 
; N -> NonTerminal symbols {}
; S -> Start Point
; P -> Production Rules :
                  ; exp -> number
                  ; exp -> symbol
                  ; exp -> + exp exp
                  ; exp -> - exp exp
                  ; exp -> * exp exp
                  ; exp -> ** exp exp
                  ; exp -> -1 * exp (The best one without problem) (Uniary Minus)
                  ; exp -> (exp)

       ;; Function definition
       ; F is a function
       ; Ls is a list of parameters
       ; B is body
       ; F -> (Name)Ls{B}
       ; Name -> symbol
       ; B-> exp
       ; Ls-> listOfSymbols

       ;; Function Application
       ;Fa is function application
       ;Fs is a function symbol
       ;La is a list of arguments
       ;Fa -> FsLa
       ;La  -> listOfSymbols
       ;Fs -> symbol


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)]
  [factaccC (x : number) (acc : number)]
  [fibC (x : number)]
  [fibaccC (x : number) (acc : number)]
  )
;Extended data definition ,,Function data definition
;; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])

;; parse : s-exp -> ExprC
;; Purpose : To parse given s-exp to ExprC form

(define (parse [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'igz (s-exp->symbol (first sl)))
              (igz (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
            [(-) (subC (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))

; Parser Tests :
(test (parse (number->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))
(test (parse '(+ 3 4))(plusC (numC 3)(numC 4)))
(test (parse '(* 3 4))(multC (numC 3)(numC 4)))
(test (parse '(+ x x))(plusC (idC 'x)(idC 'x)))
(test (parse '(* x x))(multC (idC 'x)(idC 'x)))
(test (parse '(f (* x x)))(appC 'f (multC (idC 'x)(idC 'x))))

;Contract
;;symbol (list of func definitions)-> : FunDefC
;Purpose
;; it takes a symobol and generate a function definition.

(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))
;(fdC 'triple  'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x)))

   (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))
 

 ;Contract
; Example of how substitution currently works
; (subst (plusC (numC 1) (numC 2))
;        'x
;        (plusC (idC 'x) (idC 'x)))
; =>
; (plusC (plusC (numC 1) (numC 2))
;        (plusC (numC 1) (numC 2)))
; i.e. Argument evaluation is deferred
; so it means our substitution has lazy semantics
    (define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
 
     [subC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]
     [factaccC (x fact) (factaccC (subst what for x) (subst what for fact))]
     [factC (x) (factC (subst what for x))]
     [igz (exp1 exp2 exp3) (igz (subst what for exp1) (subst what for exp2) (subst what for exp3))]))
 ;Tests for substitution
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
 ;(test (subst(numC 7 ,numC 8) '(x , y) (plusC (plusC (idC  'x) (idC  'y)) (idC 'x))) (plusC (plusC (numC 7) (numC 8)) (numC 7)))



;  lookup function takes n as a symbol and environment which includes binding values,
;  then it checks wheter this funciton in environment or not?
;  if there is,it produces value otherwise it gives error
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;Binding
;this function takes symbol as name and value which is number
;to bind any funciton
(define-type Binding
  [bind (name : symbol) (val : number)])

; An alias to work easily on Environment.
(define-type-alias Env (listof Binding))
;Empty enviorenment.
(define mt-env empty)
;Extended environment.
(define extend-env cons)


;; Interpreter 
;; Purpose : To interpreter given expression to value
;; Template : 
;  (define (interp [expr : ExprC] [env : Environment]) : number
;  (type-case
;    [n ...]
;    [id ...]
;    [lam ...]
;    [ifzero ...]
;    any function
;    ))
(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                            mt-env) fds))]
    [subC (l r) (- (interp l env fds) (interp r env fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) env fds))])]
    [igz (exp1 exp2 exp3)
             (if (= 0 (interp exp1 env fds))
                 (interp exp2 env fds)
                 (interp exp3 env fds))]
  ;;  [factaccC]
    [fibC (x) (cond 
                 [ifzero n 1
                             (ifzero (- n 1) 1
                                        (ifzero (- n 2) 1
                                                (+ (fibC (- n 1))
                                                   (fibC (- n 2)))))])]
    ))


;Tests of interpreter
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)


(test (interp (parse '(+ 10 (const5 (10))))
              mt-env
              FuncDefNameSpace) 15)
(test (interp (parse '(+ 10 (double ((+ 1 2)))))
              mt-env
              FuncDefNameSpace) 16)