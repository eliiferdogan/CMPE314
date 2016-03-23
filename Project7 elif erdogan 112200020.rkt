#lang plai-typed
;; λ-expression grammar
;; λ-calc -> v
;; λ-calc -> (λ-calc λ-calc)
;; λ-calc -> (λ v λ-calc)
;; where v is a symbol.

;; λ-calc is an abstract syntax grammar or a parse tree definition for
;; - λ-calc that defined above.
(define-type λ-calc
  (λ-sym (v : symbol))
  (λ-app (l : λ-calc)(r : λ-calc))
  (λ-def (v : symbol)(p : λ-calc))
  )

;; parse : s-exp -> λ-calc
;; Purpose : To transform given s-expression to corresponding
(define (parse (sx : s-expression)) : λ-calc
  (cond
    [(s-exp-symbol? sx)(λ-sym (s-exp->symbol sx))]
    [(s-exp-list? sx)
     (let ([sx-list (s-exp->list sx)])
       (cond
         [(= 2 (length sx-list))
          (λ-app (parse (first sx-list))(parse (second sx-list)))]
         [(= 3 (length sx-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sx-list)))
                   (s-exp-symbol? (second sx-list)))
              (λ-def (s-exp->symbol(second sx-list))
                     (parse (third sx-list)))
              (error 'parse "Not valid λ-definition")
              )]
         [else (error 'parse "Not valid length λ-calc")]
         ))]
    [else (error 'parse "Not valid λ-calc")]
    ))


;; unparse : λ-calc -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-calc)) : s-expression
  (type-case λ-calc le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))


;; substituter : λ-calc  symbol  λ-calc -> λ-calc
(define (substituter [what : λ-calc] [for : symbol] [in : λ-calc]) : λ-calc 
  (type-case λ-calc in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substituter what for l)
                        (substituter what for r)))
    (λ-def (v p)(λ-def v (substituter what for p)))
    )
  )


;; beta-transformer : ((λ x M) N) --> [M:x=N]
;; λ-calculus beta-transformation naive implementation.
(define (beta-transformer (le : λ-calc)) : λ-calc
  (type-case λ-calc le
    (λ-sym (v) le) 
    (λ-app (l r) (if (λ-def? l)
                     (substituter r (λ-def-v l) (λ-def-p l))
                     (λ-app (beta-transformer l) (beta-transformer r))))
    (λ-def (v p) (λ-def v (beta-transformer p)))))

;; A set represented as a list.
;; union : (listof symbol) (listof symbol) -> (listof symbol)
;; finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; free-identifier : λ-calc -> (listof symbol)
;; Purpose : To find free identifiers in given λ expression.
(define (free-identifier (le : λ-calc)) : (listof symbol)
  (type-case λ-calc le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

(test (union empty empty) empty)
(test (union empty (list 'x)) (list 'x))
(test (union (list 'x)(list 'x 'y)) (list 'x 'y))

(test (parse (symbol->s-exp 'y))(λ-sym 'y))
(test (parse '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parse '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))

(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
      '((λ x x) y))

(test (free-identifier (parse '(λ x x))) empty) ;;bad one
(test (free-identifier (parse '(λ x y))) (list 'y)) ;;good 
(test (free-identifier (parse '((λ x y)(λ y z)))) (list 'y 'z)) ;;good
(test (free-identifier (parse '((λ f y)(λ z z)))) (list 'y)) ;; good
(test (free-identifier (parse '((λ f y)(λ y y)))) (empty)) ;;bad one
(test (free-identifier (parse '((λ f z)(λ y z)))) (list 'y)) ;; bad one

(test (beta-transformer (parse '((λ x x) a)))
      (parse (symbol->s-exp 'a)))

(test (beta-transformer (parse '((λ x y) a)))
      (parse (symbol->s-exp 'y)))

(test (beta-transformer (parse '((λ x (a b)) k)))
      (parse '(a b)))

(test (beta-transformer (parse '((λ x (λ x y)) k)))
      (parse '(λ x y)))

(test (beta-transformer (parse '((λ x (λ y x)) k)))
      (parse '(λ y k)))

(test (beta-transformer (parse '((λ x (λ y (x y))) k)))
      (parse '(λ y (k y))))

(define zero '(λ f
  (λ x
    x)))
(define one
  '(λ f
    (λ x
      (f x))))
(define two
  '(λ f
    (λ x
      (f (f x)))))
(define succ '(λ n (λ f (λ x (f ((n f) x))))))
(define (church->number n) ((n add1) 0))
(define add '(λ n (λ m (λ f (λ x ((n f) ((m f) x)))))))
(beta-transformer(parse add))
;;(parse two)
;;(define (add one two) '(λ f (λ x ((one f) ((two f) x))))) 
;;(define five (add one two))
;;(beta-transformer (parse five))