#lang pl 02

#|
Question 1 -- BNF for LE (List Expressions) language
Total solving time: 1 hr
;; Description:
This question was challenging, the expamples of the valid/invalid
language expressions assisted us with building our BNF for the language.

;; Main difficulties:
Formalizing our BNF - We needed to perform a derivation process of a few annoying
example expressions to see if our BNF accepts them.
________________________________________________________________________________________________________________________________________________________

(a) Our BNF for <LE> language:

<LE> ::= {<num>}                 (1) A Racket `Number` terminal                                 
         |{list <LE...>}         (2) A list expression which accepts any <LE>                                       
         |{cons <LE> <list>}     (3) An expression which evaluates to a list and accepts any LE argument as the first argument and a list as the second. 
         |{append <list...>}     (4) An expression which accepts 0 or more list arguments and evaluates to a list.
         |{' <sym>}              (5) A `quote` (') symbol which operates on racket symbols (<sym>)
         |{<null>}               (6) A null terminal which represents an empty list
         |{<sym>}                (7) A Racket `Symbol` terminal
                         

1) 311 - It - ay - Sh

     (cons 311 (cons (append (cons 'Sh
      null) (list 'It 'ay)) null))

                cons                    
               /    \              =>(3)
           <num>     <cons>                 
            /        /    \        =>(1) =>(3)
          311   <append>     <null>  
                /     \            =>(4)
           <cons>     <list>
            /  \        /  \       =>(3) =>(2)
          <'> <null>  <'>  <'>     =>(6)
          /           /      \     =>(5) =>(5) =>(5)
       <sym>       <sym>     <sym>
        /           /           \  =>(7) =>(7) =>(7)
      'Sh         'It           'ay

2) 315 - Gil - Tzi

    (cons 'Gil (append (list 'Tzi) (list 315)))

                cons                    
               /    \              =>(3)
           <'>     <append>                 
            /        /    \        =>(5) =>(4)
          <sym>   <list>   <list>  
                    /        \     =>(5) =>(2)
                   <'>       <num>              
                    /          \   =>(7) =>(1)
                 <sym>         315

3) It - ay - Gil

  (list 'It 'ay 'Gil)

           list

         /   |   \          =>(2)
      <'>   <'>   <'>   
      /      |      \       =>(5) =>(5) =>(5)
   <sym>   <sym>    <sym>     
    /        |         \    =>(7) =>(7) =>(7)
  'It       'ay       'Gil
_________________________________________________________________________________________________________
|#

#| Question 2 -- Higher Order Functions

Total solving time: 5 min
;; Description:

a ) We've relized quickly that in order to turn the AE language to `infix`
all we need is to move the operator (`+`, `-`, ...) to in-between the operands and mark
our changes with comments.

b ) The only modification to `eval` function was
adding an if statement in the case of a division expression ([Div x y])
to see if the right operand of the division action is 0, and return 999.
;; Main difficulties:
No difficulties.
|#


#|
The AE grammer

  <AE> ::= <num>
           | { <AE> + <AE> }
           | { <AE> - <AE> }
           | { <AE> * <AE> }
           | { <AE> / <AE> }
|#
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)] 
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))] ;; `+` sign is moved to index 1 in the list of arguments
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))] ;; `-` sign is moved to index 1 in the list if arguments
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))] ;; `*` sign is moved to index 1 in the list if arguments
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))] ;; `/` sign is moved to index 1 in the list if arguments
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)])) ;; A `bad syntax` error.


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose infix form grammer with curly parentheses
(test (parse "{3 + 4}") => (Add (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{{3 - 2} + 4 }") => (Add (Sub (Num 3)
                                              (Num 2))
                                         (Num 4)))
(test (parse "{1 + 2 + 3 + 4}") =error> "bad syntax")



#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr

|#



#|
;;; ====== EVAL  ==============
; <AE> ::= <num>               a 
;          | { + <AE> <AE> }   b
;          | { - <AE> <AE> }   c

eval(<num>) = <num>
eval({+ E1 E2}) =  eval(E1) + eval(E2)
eval({- E1 E2}) =  eval(E1) - eval(E2)
|#



(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (cond
                 [(zero? (eval r)) 999]  ;; Our modification - check if right operand of the division is 0
                 [else (/ (eval l) (eval r))])]))


(: run : String -> Number)
(define (run code)
  (eval (parse code)))

;; Tests for Q2
(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)
(test (eval (parse "{3 + 4}")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{3 - 2} + 4 }")) => 5)
(test (eval (parse "{1 + 2 + 3 + 4}")) =error> "bad syntax")
(test (eval (parse "{+ 1 2}")) =error> "bad syntax")
(test (eval (parse "{- 1 2}")) =error> "bad syntax")
(test (eval (parse "{* 1 2}")) =error> "bad syntax")
(test (eval (parse "{/ 1 2}")) =error> "bad syntax")
(test (eval (parse "{3 * {5 / 3}}")) => 5)
(test (eval (parse "{3 * 5 + 3}")) =error> "bad syntax")
(test (eval (parse "{0 / 3}")) => 0)
(test (eval (parse "{5 / 0}")) => 999)
(test (eval (parse "{0 / 0}")) => 999)
(test (run "{3 + 4}") => 7)
(test (run "3") => 3)
(test (run "{{3 - 2} + 4 }") => 5)
(test (run "{1 + 2 + 3 + 4}") =error> "bad syntax")




#| Question 3 -- Higher Order Functions

Total solving time: 30 min
;; Description:
The solution uses `foldl` and `map` to apply functionalities to lists and their items.
We use a `Combiner function` `+` and sum out the result of squaring each number in the list.

;; Main difficulties:
The most challenging thing in solving this question was getting to know the functions we were asked to use
(foldl and map).
After that, it was pretty easy to solve the question and understand what parameters I sould send to the functions.
It took something like 30 minutes to solve it from start to finish. 
|#

(: sum-of-squares : (Listof Number) -> Number)
#|
   'sum-of-squares' recive a Listof Numbers, and returns a number that represents the sum of the squares of all of the numbers in the list.
    this one-liner function uses the 'map' function to run 'square' procedure on the items of the input list, and 'foldl' function to sum all the results (squares). 
 |#

;; 
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

(: square : Number -> Number)
#|
  'square' consumes a number and returns the number squared.
   a one-liner function, that helps on the sum-of-squares function. 
|#
(define (square num)
  (* num num))

;; tests
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(-1)) => 1)
(test (sum-of-squares '(2 2 3)) => 17)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '(3 3 1)) => 19)
(test (sum-of-squares '(-1 -2 0)) => 5)

#| ______________________________________________________________________________________________________|#

#| Question 4 -- Typed Racket

Total solving time: 10 min
;; Description:
This question was well explained and easy to solve.
We think the functions documentation explains the solution well enough.
a) Defining a type [BINTREE]
   which contains either Nodes, or Leaf (leaves.

b) Defining and implementing a function `tree-map`
which is explained below.
;; Main difficulties:
None
|#

(define-type BINTREE
[Node BINTREE BINTREE]
[Leaf Number]
)
#|
`tree-map` function:
A `Higher order` function which consumes a numeric function `f: N->N`
and a `BINTREE` we've defined in (a).
The `tree-map` applies function `f` on all the Leaf types of the BINTREE (Not `Nodes`).
The return value of `tree-map` is the same BINTREE consumed, but each value, `n`, in its leaves
will be `f(n)` instead of `n`.
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f bt)
   (cases bt
     [(Node l r) (Node (tree-map f l)(tree-map f r))]
     [(Leaf n) (Leaf (f n))]
     ))

(test (tree-map add1
(Leaf 0))
 => (Leaf 1))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-map add1
(Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))))
 => (Node (Leaf 2) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))))