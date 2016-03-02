#lang plai-typed

;;ExprC 
;;S->number  S->symbol  S->S    S->S+ S     S->S*S   S->F  S->ε
; S->( λ(list of symbol) (S))  S->(S S S)

;S→F(a,b,c)   F(x,y,z)→xyz
;The grammar above is equivalent to the following because x, y, and z are substituted:
;S→abc
;Now suppose we leverage the function capability with the following grammar:
;S→F(ε)          F(x)→x a | x a F(x b)         (ε means empty string)

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

;;FMP - Language with First Class Multi Parameter Functions
;;Grammar of FMP
;;FMP -> number
;;FMP -> symbol
;;FMP -> (+ FMP FMP)
;;FMP -> (- FMP FMP)
;;FMP -> (* FMP FMP)
;;FMP -> (λ (listof symbol) (FMP))
;;FMP -> (FMP FMP FMP)
;;FMP Data Definition

(define-type FMP
  [FMPnum (n : number)]
  [FMPadd (lhs : FMP) (rhs : FMP)]
  [FMPsub (lhs : FMP) (rhs : FMP)]
  [FMPmul (lhs : FMP) (rhs : FMP)]
  [FMPid (s : symbol)]
  [FMPlam (params : (listof symbol)) (body : FMP)]
  [FMPifzero (pred : FMP)(truestate : FMP)(falsestate : FMP)])
;;Defines datatype for function definitions
;;function definitions have a name, one argument, and a body
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


;; parse s-expression -> ExprC
;; convert a quoted s expression into the equivalent ArithC form
;; examples
;;  '(+ 23 (+ 23 5)))-> (plusC (numC 23)(plusC (numC 23) (numC 5))))
;; (symbol->s-exp 'x))->  (idC 'x))
;; '(if 1 2 3)->(ifC (numC 1) (numC 2) (numC 3)))
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(u-) (uminusS (parse (second sl)))]
         [else (appS (s-exp->symbol (first sl))
                     (parse (second sl)))]))]
    [else (error 'parse "invalid input")]))
"Examples of Parse"

(test (parse '(* 12 7)) (multS (numS 12) (numS 7)))

(test (parse '(+ 23 (+ 23 5)))
      (plusS (numS 23)
            (plusS (numS 23) (numS 5))))
(test (parse (symbol->s-exp 'x)) (idS 'x))

(test (parse '(double 13))
      (appS 'double (numS 13)))
;(test(parse '(if 1 2 3))(ifC (numC 1) (numC 2) (numC 3)))

(test (parse '(double (+ 2 (+ 1 1))))
(appS 'double (plusS (numS 2) (plusS (numS 1) (numS 1)))))

(define (parsedef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(define) (fdC (s-exp->symbol (first (s-exp->list (second sl))))
                        (s-exp->symbol (second (s-exp->list (second sl))))
                        (desugar (parse (third sl))))]
         [else (error 'parsedef "invalid list")]))]
    [else (error 'parsedef "invalid input")]))


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1)
                        (desugar e))]
    [idS (s) (idC s)]
    [appS (fun arg) (appC fun (desugar arg))]))

;; substitution of replacing a name in an expr with another expr 
;; what = what we want to replace the name with
;; for = what name we want to perform substitution
;; in = in which expression we want to do it

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
                        (subst what for r))]))

;;Defines datatype for function definitions
;;function definitions have a name, one argument, and a body
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;;lookup function takes n as a symbol and environment which includes binding values,
;; then it checks wheter this funciton in environment or not?
;;if there is,it produces value otherwise it gives error
(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "Symbol not found in env")]
    [(cons? env) (cond
                   [(equal? n (bind-name (first env))) (bind-val (first env))]
                   [else (lookup n (rest env))])]))

;Binding
;this function takes symbol as name and value which is number
;to bind any funciton
(define-type Binding
  [bind (name : symbol) (val : number)])

;; An alias to work easily on Environment.
(define-type-alias Env (listof Binding))

;; Empty environment.
(define mt-env empty)

;; Extending environment
(define extend-env cons)


(define-type Value
  [numvalue (n : number)]
  [functionvalue (params : (listof symbol)) (body : FMP) (env : Env)])

;; interp : ExprC (listof FunDefC) -> number


;; Interpreter 
;; Purpose : To interpreter given FMP to value
;; Template : 
;(define (interp [expr : FMP] [env : Environment]) : Value
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
                                      mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))
 ; [ifC (pred t f)
  ;           (if (= 0 (numC (interp pred env)))
   ;             (interp t env   (interp f env)))])
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
;; parse : s-exp -> FMP
;; Purpose : To parse given s-exp to abstract syntax FMP
;; Template : 
;(define (parse [s : s-expression]) : FMP
;  (cond
;    [n ...]
;    [id ...]
;    [add ...]
;    [sub ...]
;    ))

(define (parse2 [s : s-expression]) : FMP
  (cond
    [(s-exp-number? s) (FMPnum (s-exp->number s))]
    [(s-exp-symbol? s) (FMPid (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(and(= (length sl) 3)
          (symbol=? (s-exp->symbol (first sl)) '+))
          (FMPadd (parse2 (second sl)) (parse2 (third sl)))]   
         [(and(= (length sl) 3)
          (symbol=? (s-exp->symbol (first sl)) '-))
          (FMPsub (parse2 (second sl)) (parse2 (third sl)))]
         [(and(= (length sl) 3)
          (symbol=? (s-exp->symbol (first sl)) '*))
          (FMPmul (parse2 (second sl)) (parse2 (third sl)))]
         [(and (= (length sl) 3) 
          (symbol=? (s-exp->symbol (first sl)) 'λ))
          (FMPlam (map (lambda (x) (s-exp->symbol x))
                       (s-exp->list(second sl)))
                  (parse2 (third sl)))]
         [(= (length sl) 4)
          (case (s-exp->symbol (first sl))
            [(ifzero) (FMPifzero 
                       (parse2 (second sl))
                       (parse2 (third sl))
                       (parse2 (fourth sl)))]
            [else (error 'parse2 "invalid keyword !")]
            )]              
         [else (error 'parse2 "invalid list input")])
       )]
    [else (error 'parse2 "invalid input")]))