#| Itay Sharabi |#
#lang pl





#|Question 1: |#
(: min&max : Number Number Number Number Number -> (Listof Number))
(define (min&max x1 x2 x3 x4 x5)
  (list (min x1 x2 x3 x4 x5) (max x1 x2 x3 x4 x5)))

(test (min&max 1 2 3 -1 -2) => '(-2 3))

#|Question 2: |#

; Q 2.a:

(: sublist-numbers : (Listof Any) -> (Listof Number))
(: sublist_help : (Listof Any) -> (Listof Number))

(define (sublist-numbers lst)
  (if (null? lst) null (sublist_help lst)))

(define (sublist_help lst)
  (cond
    [(null? lst) null]
    [(equal? (number? (first lst)) #t) (cons (first (filter number? (list (first lst)))) (sublist_help (rest lst)))]
    [else (sublist_help (rest lst))]))


;; Tests: 
(test (sublist-numbers (list 1 2 3 -1 -2)) => '(1 2 3 -1 -2))
(test (sublist-numbers (list 1 #t 3)) => '(1 3))
(test (sublist-numbers (list #t #t #t #t #t)) => '())
(test (sublist-numbers (list )) => '())
(test (sublist-numbers null) => '())
(test (sublist-numbers (list 'any "Benny" 10 'OP 8))
 => '(10 8))
(test (sublist-numbers '(any "Benny" OP (2 3)))
 => null)

; Q 2.b:

(: min&max-lists : (Listof (Listof Any))-> (Listof (Listof Number)))
(define (min&max-lists lst)
  (cond
    [(null? lst) lst]
    [(< 0 (length (sublist-numbers (first lst)))) (cons (list (apply min (sublist-numbers (first lst))) (apply max (sublist-numbers (first lst)))) (min&max-lists (rest lst)))]
    [else (cons (list ) (min&max-lists (rest lst)))]
    ))


(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))


;;Tests for 2.b

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3))))
 => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ()))
 => '((1 5) (1 7) ()))
(test (min&max-lists '((any "Benny" OP) (any "Benny" OP (2 3))))
 => '(() ()))
(test (min&max-lists '((2 5 1 5 2 1) (4 100 0 6 -1 3 2 1) ()))
 => '((1 5) (-1 100) ()))
(test (min&max-lists '((1) ("dks" 'f (1 3 4)))) => '((1 1) ()))
(test (min&max-lists '((0 "3" "sdf" 'e) (0 "3" "sdf" '0 0)))
 => '((0 0) (0 0)))
(test (min&max-lists '((-2 1 -3 2 5) (4 4 0 6 5 2) (6 4 0 5 2)))
 => '((-3 5) (0 6) (0 6)))
(test (min&max-lists '((any "Benny" 10 OP 8 13) (any -2 10 OP 8 -13 12)))
 => '((8 13) (-13 12)))
(test (min&max-lists '((any "Benny" O) (any OP (8 -1 "4"))))
 => '(() ()))
(test (min&max-lists '(()))
 => '(()))
(test (min&max-lists '((1 2 3)))
 => '((1 3)))

#|Question 3|#


(define-type KeyStack
  [EmptyKS ]
  [Push Symbol String KeyStack]
  )

(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symb stk)
  (cases stk
    [(EmptyKS) #f]
    [(Push sy str ks) (if (equal? sy symb) str (search-stack symb ks))]
    )
  )



(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack stk)
  (cases stk
    [(EmptyKS) #f]
    [(Push k v KS)
     KS]
    )
  )

;; Tests
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
 (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => "AAA")

(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS)))
      => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'b "B" (EmptyKS))
      => (Push 'b "B" (EmptyKS)))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
      => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (Push 'b "B" (Push 'a "A" (EmptyKS))))))
      => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (Push 'b "B" (Push 'a "A" (EmptyKS)))))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
      => "AAA")
(test (search-stack 'a (Push 'b "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
      => "A")
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
      => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
      => #f)

(test (search-stack 'a (Push 'b "AAA" (Push 'b "B" (Push 'g "A" (Push 'b "B" (Push 'a "hi" (EmptyKS)))))))
      => "hi")