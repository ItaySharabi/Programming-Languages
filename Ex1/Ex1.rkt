#| Itay Sharabi |#
#lang pl

;; ===============================================================================================================
#|Question 1: |#
#|
Function `min&max`:
Consumes a sequence of 5 Numbers: `x1, x2, ..., x5`,
and outputs a list (Listof Number) of size 2, which contains the minimum and maximum of `x1, ..., x5`. 
|#
(: min&max : Number Number Number Number Number -> (Listof Number))
(define (min&max x1 x2 x3 x4 x5)
  (list (min x1 x2 x3 x4 x5) (max x1 x2 x3 x4 x5)))

(test (min&max -2 -1 0 1 2) => '(-2 2))
(test (min&max 1 1 1 1 1) => '(1 1))
(test (min&max 1 2 -1 -1 2) => '(-1 2))
(test (min&max -100 -99 -88 -77 -1) => '(-100 -1))


;; ===============================================================================================================

#|Question 2: |#

; Q 2.a:

#|
Function `sublist-numbers`:
Consumes a list of any type (Listof Any), `lst`,
and outputs a list (Listof Number) of all numbers found in `lst`. 
|#
(: sublist-numbers :
   (Listof Any) -> (Listof Number))

(define (sublist-numbers lst)
  (if (null? lst)
      null  ; if null
      (sublist_help lst (rest (list 1))))) ; Using (rest (list 1)) in order to get a reference to an empty (Listof Number)




#|
Function `sublist_help`:
Consumes:
    The original given list (Listof Any) `lst`
    An empty list (Listof Number), `ret_val`.

A `Tail-Recursion` function which builds up the return value ((Listof Number)) `ret_val`
down the recursive calls and eventually returns it.
|#
(: sublist_help : (Listof Any) (Listof Number) -> (Listof Number))

(define (sublist_help lst ret_val)
  (cond
    [(null? lst) ret_val]
    [(equal? (number? (first lst)) #t) (append
                                        (list (first (filter number? (list (first lst)))))
                                        (sublist_help (rest lst) ret_val)
                                        )
                                       ]
    [else (sublist_help (rest lst) ret_val)]))



;; Tests for 2.a: 
(test (sublist-numbers (list -2 2)) => '(-2 2))
(test (sublist-numbers (list 1 'tag 2)) => '(1 2))
(test (sublist-numbers (list #t '1 "2")) => '(1))
(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(10 8))
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers (list )) => '())
(test (sublist-numbers null) => '())

; Q 2.b:
#|
Function `min&max-lists`, consumes (Listof (Listof Any)),
a List, `lst`, which contains elements of type List (`inner_lst`), which contains elements of any type,
and outputs a List, which contains Lists of numbers out of the original input `lst`.
The output list contains in its i'th element:
1. Empty list - if the i'th list contains 0 numbers.
2. A list of size 2 - Otherwise (size >= 1).
   The list contains the minimum and maximum numbers out of the i'th list.

Calls `sublist-numbers` to extract numbers out of given lists, and applies (apply min/max (Listof Number)) functionalities of Racket.
|#
(: min&max-lists : (Listof (Listof Any))-> (Listof (Listof Number)))

(define (min&max-lists lst)
  (cond
    [(null? lst) lst]
    [(< 0 (length (sublist-numbers (first lst)))) ; if len(sublist-nums) >= 1
     (cons ; Construct:
      (list ; a list - containing the min and max out of the numbers of the `inner_list` (first lst)
            (apply min (sublist-numbers (first lst)))
            (apply max (sublist-numbers (first lst))))
      (min&max-lists (rest lst))) ; with the rest of the list in a recursive manner
     ]
    [else (cons (list ) (min&max-lists (rest lst)))] ; Otherwise (len == 0) - append an empty list '().
    )
  )


(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(() (4 5 6 7 3 2 L) ("2" "3"))) => '(() (2 7) ()))
(test (min&max-lists (list '(1) '(2))) => '((1 1) (2 2)))
(test (min&max-lists (list '(1) (list 2))) => '((1 1) (2 2)))
(test (min&max-lists (list '(1 1 1 1 1 1 1) '(2 2 2 2 2 2))) => '((1 1) (2 2)))
(test (min&max-lists null) => null)

;;Tests for 2.b

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3))))
 => '((8 10) ()))
(test (min&max-lists '((1 L 5 2 2 2) (1 2 1 2 5 6 7) (L R L R L)))
 => '((1 5) (1 7) ()))
(test (min&max-lists '((any "Benny" OP) (any "Benny" OP (2 3))))
 => '(() ()))


;; ===============================================================================================================
#|Question 3|#

#|
Define a KeyStack "object":
This will behave as a Stack data-structure which associates keys to values.
Keys are symbols and values are Strings.
|#
(define-type KeyStack
  [EmptyKS ] ; Empty constructor
  [Push Symbol String KeyStack] ; Constructor which consumes (Key, Value, Stack) parameters
  )

#|
Function `search-stack`:
Consumes:
    Symbol `symb` - A symbol (Key) to search for in the stack
    Stack `stk` - A KeyStack to search in.
Returns:
    #f (False) - if not found.
    Otherwise - returns the first value (String) associated with the symbol that was found
|#
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symb stk)
  (cases stk
    [(EmptyKS) #f]
    [(Push sy str ks)
     (if (equal? sy symb)
         str ; if found symbol - return the string associated with it.
         (search-stack symb ks))] ; else - keep searching recursively
    )
  )


#|
Function `pop-stack`:
Consumes:
    Stack `stk` - A KeyStack to pop an item from.
Returns:
    #f (False) - if the Keyed Stack is empty.
    Otherwise - returns the first value (String) associated with the symbol that was found
|#
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack stk)
  (cases stk
    [(EmptyKS) #f] ; If the stack is empty
    [(Push k v KS) ; If the stack has an item:
     KS]           ; Return only the rest of the stack (KS)
    )
  )

;; Tests for KeyStack:
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
 (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => "AAA")

(test (search-stack 'c (Push 'a "A" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "C" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

(test (EmptyKS) => (EmptyKS))
(test (Push 'a "B" (EmptyKS))
      => (Push 'a "B" (EmptyKS)))


(test (Push 'b "A" (Push 'a "B" (EmptyKS)))
      => (Push 'b "A" (Push 'a "B" (EmptyKS))))

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