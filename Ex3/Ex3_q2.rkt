#lang pl
#|

<LE> ::=   { <num> }             (1) A Racket `Number` terminal                                 
         | { list <LE...> }      (2) A list expression which accepts any <LE>                                       
         | { cons <LE> <list> }  (3) An expression which evaluates to a list and accepts any LE argument as the first argument and a list as the second. 
         | { append <list...> }  (4) An expression which accepts 0 or more list arguments and evaluates to a list.
         | { ' <sym> }           (5) A `quote` (') symbol which operates on racket symbols (<sym>)
         | { <null> }            (6) A null terminal which represents an empty list
         | { <sym> }             (7) A Racket `Symbol` terminal

|#

;; LE abstract syntax trees 
(define-type LE = (U LIST ATOM))

;; LIST abstract syntax trees 
(define-type LIST
  [ListLE (Listof LE)]
  [Cons LE LIST]
  [Append (Listof LIST)]
  [EmptyList ])
;; ATOM abstract syntax trees 
(define-type ATOM
  [NumLE Number]
  [SymLE Symbol]) 


;; LE Language Parser:
#|
The parser implementation was quite easy.
We've taken the parser skeleton from Moodle and
filled in the blanks. Thank you for the partial solution!
This parser implementation follows exactly our BNF from Ex2.

Time: 20 min (Once understanding what and how to accomplish).
|#
(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE)) 
 ;; converts a list of s-expressions into a list of LEs 
 (define (parse-sexpr->LEs sexprs) 
 (map parse-sexprLE sexprs))

(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST)) 
;; converts a list of s-exprs into a list of LISTs 
 (define (parse-sexpr->LISTs sexprs)
   (map parse-sexpr->LIST sexprs))


(: parse-sexpr->LIST : Sexpr -> LIST) 
 (define (parse-sexpr->LIST sexpr) 
 (let ([ast (parse-sexprLE sexpr)]) 
 (if (LIST? ast)
     ast
     (error 'parsesexprLE "expected LIST; got ~s" ast))))


(: parse-sexprLE : Sexpr -> LE) 
;; to convert s-expressions into LEs 
(define (parse-sexprLE sexpr) 
 (match sexpr 
 [(number: n) (NumLE n)] 
 ['null (EmptyList )] 
 [(symbol: s) (SymLE s)] 
 [(cons 'list rest) 
  (cond
    [(null? rest) (EmptyList )]
    [else (ListLE (parse-sexpr->LEs rest))])] 
 [(list 'cons le list-sexpr)
  (Cons (parse-sexprLE le) (parse-sexpr->LIST list-sexpr))]
 [(list 'append) 
  (EmptyList )]
 [(list 'append le)
  (Append (parse-sexpr->LISTs (list le)))] 
 [else (error 'parse-sexprLE "bad syntax in ~s" sexpr)])) 
 
(: parseLE : String -> LE) 
 ;; parses a string containing a LE expression to a 
 ;; LE AST 
(define (parseLE str) 
 (parse-sexprLE (string->sexpr str)))

(test (parseLE "{cons 1 null}") =>
      (Cons (NumLE 1) (EmptyList )))

(test (parseLE "null") => (EmptyList))
(test (parseLE "{append}") => (EmptyList))
(test (parseLE "{list}") => (EmptyList))
 


(test (parseLE "{append {cons 1 {append}}}") =>
      (Append (list (Cons (NumLE 1) (EmptyList)))))

(test (parseLE "{cons 1 2}") =error> "expected LIST")
(test (parseLE "{cons}") =error> "bad syntax*")



#| ================== evalLE ==================

Evaluation procedure for our LE Language -
consuming an LE abstract syntax tree and
evaluates the given program, contained in a String.

The task of writing an interpreter for our language
took less than 2 hours (Thanks to the partial solution given to us).
|#

#|
Formal specs for `eval`: 
eval(N) = N ;; for numbers 

eval(Sym) = 'Sym ;; for symbols 

eval({list E ...}) = (list eval(E) ...)

eval({cons E1 E2}) = if eval(E2) = (list E), then 
                       (cons eval(E1) eval(E2)) 
                     else error!

eval({append E ...}) = if for all expressions E:
                           eval(E) = (list E'), then
                          (append eval(E) ...)
|# 


(: eval-append-args : (Listof LE) -> (Listof (Listof Any))) 
 ;; evaluates LE expressions by reducing them to lists 
 (define (eval-append-args exprs) 
 (if (null? exprs)
     null
     (let ([fst-val (evalLE (first exprs))])
       (if (list? fst-val)
           (cons fst-val (eval-append-args (rest exprs)))
           (error 'evalLE "append argument: expected List got ~s" fst-val)))))

 (: evalLE : LE -> Any)
 ;; evaluates LE expressions by reducing them to numbers, symbols and lists
 (define (evalLE expr)
   (if (LIST? expr)
       (cases expr
         [(ListLE les) (map evalLE les)] 
         [(Cons first rest)
          (cons (evalLE first)
                (let
                  ([rst-val (evalLE rest)])
                  (if (list? rst-val) rst-val (error 'evalLE "expected list"))))]
         [(Append listof-les) (apply append (eval-append-args listof-les))]
         [(EmptyList) null])
       
       ;; else - if ATOM:
       (cases expr
         [(NumLE n) n]
         [(SymLE s) s])))
 
 (: runLE : String -> Any) 
 ;; evaluate a LE program contained in a string 
 (define (runLE str)
   (evalLE (parseLE str)))



(test (runLE "null") =>
      null)

(test (runLE "12") =>
      12)

(test (runLE "boo") =>
      'boo)

(test (runLE "{cons 1 {cons two null}}") =>
      '(1 two))

(test (runLE "{cons 1 {append {append}}}") =>
      '(1))

(test (runLE "{list 1 2 3}") =>
      '(1 2 3))

(test (runLE "{cons 1 {cons 2 {cons 3 {append}}}}") =>
      '(1 2 3))

(test (runLE "{append {cons 1 {append}}}") => '(1))


(test (runLE "{list {cons}}") =error>
      "parse-sexprLE: bad syntax in (cons)")

(test (runLE "{list {cons 2 1}}") =error>
      "parsesexprLE: expected LIST; got")
