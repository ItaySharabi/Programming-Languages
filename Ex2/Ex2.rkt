#lang pl 02

#|
Question 1 -- BNF for SE (String Expressions) language
_____________________________________________________________________________________________________________________________________________

(a) the BNF:

<LE> ::= {<num>}                                      (1)                                           
         |{list <LE...>}                              (2)                                         
         |{cons <LE> <list>}                          (3)
         |{append <list...>}                          (4)                      
         |{' <sym>}                                   (5)
         |{<null>}                                    (6)
         |{<sym>}                                     (7)
                         

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

3) 'It - 'ay - 'Gil

   (list 'It 'ay 'Gil)

           list

         /   |   \ 
      <'>   <'>   <'>   
      /      |      \
   <sym>   <sym>    <sym>     
    /        |         \
  'It       'ay       'Gil
_________________________________________________________________________________________________________
|#

#| Question 2 -- Higher Order Functions |#


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
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose prefix form grammer with curly parentheses
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
                 [(equal? (eval r) 0) 999]
                 [else (/ (eval l) (eval r))])]))


(: run : String -> Number)
(define (run code)
  (eval (parse code)))


(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)

(test (eval (parse "{3 + 4}")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{3 - 2} + 4 }")) => 5)
(test (eval (parse "{1 + 2 + 3 + 4}")) =error> "bad syntax")

(test (eval (parse "{3 * {5 / 3}}")) => 5)
(test (eval (parse "{3 * {5 / 0}}")) => 999)
(test (eval (parse "{0 / 3}")) => 0)
(test (eval (parse "{5 / 0}")) => 999)
(test (eval (parse "{0 / 0}")) => 999)
(test (run "{3 + 4}") => 7)
(test (run "3") => 3)
(test (run "{{3 - 2} + 4 }") => 5)
(test (run "{1 + 2 + 3 + 4}") =error> "bad syntax")

#| Question 3 -- Higher Order Functions |#

(: sum-of-squares : (Listof Number) -> Number)
#|
   'sum-of-squares' recive a Listof Numbers, and returns a number that represents the sum of the squares of all of the numbers in the list.
    this one-liner function uses 'map' function to run 'square' procedure on the items of list, and 'foldl' function to sum all the results (squares). 
 |#

(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

(: square : Number -> Number)
#|
  'square' recive a number and returns the number squared.
   a one-liner function, that helps on the sum-of-squares function. 
|#
(define (square num)
  (* num num))

;; tests
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(2 2 3)) => 17)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '(3 3 1)) => 19)
(test (sum-of-squares '(-1 -2 0)) => 5)
(test (sum-of-squares '(-1)) => 1)

#|
The most challenging thing in solving this question was getting to know the functions we were asked to use
(foldl and map).
After that, it was pretty easy to solve the question and understand what parameters I sould send to the functions.
It took something like 30 minutes to solve it from start to finish. 
|#

#| ______________________________________________________________________________________________________|#

#| Question 4 -- Higher Order Functions |#

(define-type BINTREE
[Node BINTREE BINTREE]
[Leaf Number]
)
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f bt)
   (cases bt
     [(Node l r) (Node (tree-map f l)(tree-map f r))]
     [(Leaf n) (Leaf (f n))]
    ;; [else (error 'tree-map "bad syntax in ~s" bt)]
     ))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))