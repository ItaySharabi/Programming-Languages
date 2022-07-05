#lang pl

;;            Question 1:
;; ===================================

 #| BNF for the AE language: 
 <AE> ::= <num> 
 | { <AE> <AE> + } 
 | { <AE> <AE> - } 
 | { <AE> <AE> * } 
 | { <AE> <AE> / }
 | { <AE> <AE> power }
 | { <AE> <AE> sqr }

|# ;; AE abstract syntax trees

 (define-type AE 
 [Num Number] 
 [Add AE AE] 
 [Sub AE AE] 
 [Mul AE AE] 
 [Div AE AE]
 [Power AE AE]
 [Sqr AE])

;; Auxiliary operators `power` and `pow`:
(: pow : Number Integer -> Number) ;; Computes `base` raised to the power of `exponent`
(define (pow base exponent)
  (cond
    [(> exponent 0) (* base (pow base (- exponent 1)))]
    [(eq? exponent 0) 1]
    [else (pow (/ 1 base) (* -1 exponent))])) ;; exponent < 0


(: power : Number Number -> (U Number #f))
;; Casts the second Number argument to an Integer and
;; computes the exponent: `base` raised to the power of `exp`.
;; If casting fails - returns #f (False)
(define (power base exp)
  (cond
    [(integer? exp) (pow base exp)]
    [else #f]))

;; `pow` tests:
(test (pow 2 5) => 32)
(test (pow 2 1) => 2)
(test (pow 2 -1) => 1/2)
(test (pow 2 0) => 1)
(test (pow 2 -5) => 1/32)
;; `power` tests:
(test (power 2 4) => 16)
(test (power 2 4.5) => #f)


 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs 
 (define (parse-sexpr sexpr) 
 (match sexpr 
 [(number: n) (Num n)] 
 [(list lhs rhs '+) 
(Add (parse-sexpr lhs) (parse-sexpr rhs))] 
 [(list lhs rhs '-) 
(Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
 [(list lhs rhs '*) 
(Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
 [(list lhs rhs '/)
(Div (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list base exponent 'power)
(Power (parse-sexpr base) (parse-sexpr exponent))]
 [(list expr 'sqr)
(Sqr (parse-sexpr expr))]
 [else 
(error 'parse-sexpr "bad syntax in ~s" sexpr)]))

 (: parse : String -> AE) 
;; parses a string containing an AE expression to AE AST 
 (define (parse str) 
 (parse-sexpr (string->sexpr str)))


;; `parse` tests:
(test (parse "{{3 4 +} {1 1 +} power}") => (Power (Add (Num 3) (Num 4)) (Add (Num 1) (Num 1))))
(test (parse "{{4 4 *} sqr}") => (Sqr (Mul (Num 4) (Num 4))))
(test (parse "{3 4 +}") => (Add (Num 3) (Num 4)))
(test (parse "{3 4 -}") => (Sub (Num 3) (Num 4)))
(test (parse "{3 4 *}") => (Mul (Num 3) (Num 4)))
(test (parse "{8 4 /}") => (Div (Num 8) (Num 4)))
(test (parse "{8 + 4}") =error> "bad syntax*")


 (: eval : AE -> Number)
 ;; consumes an AE and computes the corresponding number 
 (define (eval expr) 
 (cases expr 
 [(Num n) n] 
 [(Add l r) (+ (eval l) (eval r))] 
 [(Sub l r) (- (eval l) (eval r))] 
 [(Mul l r) (* (eval l) (eval r))] 
 [(Div l r) (/ (eval l) (eval r))]
 [(Power base exp)
  (let ([result (power (eval base) (eval exp))])
    (if (eq? #f result)
        (error 'eval "Exponent should be an integer, got ~s" exp)
        result))]
 [(Sqr n) (pow (eval n) 2)]))

;; `eval` tests:
(test (eval (Mul (Num 2) (Num 6))) => 12)
(test (eval (Sub (Num 2) (Num 6))) => -4)
(test (eval (Power (Num 2) (Num 6))) => 64)
(test (eval (Power (Add (Num 1) (Num 1)) (Num 6))) => 64)
(test (eval (Sqr (Add (Num 1) (Num 1)))) => 4)
(test (eval (Power (Num 2) (Num 6.5))) =error> "Exponent*")


(: run : String -> Number) ;; evaluate an AE program contained in a string
(define (run str) 
(eval (parse str)))

;; `run` tests 
(test (run "3") => 3) 
(test (run "{3 4 +}") => 7)
(test (run "{{2 2 power} 2 /}") => 2)
(test (run "{{2 sqr} 2 /}") => 2)
(test (run "{{2 -2 power} 2 /}") => 1/8)
(test (run "{{2 4/5 power} {5 sqr} +}") =error> "eval:*")

