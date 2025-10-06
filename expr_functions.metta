;; expression function which returns size of expression
;; size is defined as number of nodes in the expression tree
(: expr-size (-> Expression Number))
(= (expr-size $expr)
  (case $expr
    (
      (($op $arg1 $arg2)
       (+ (+ 1 (expr-size $arg1)) (expr-size $arg2)))  
      (($op $arg1)
       (+ 1 (expr-size $arg1)))
      ($atom 1)
    )
  )
)

! (expr-size (implies (human Socrates) (mortal Socrates))) ;Result: 5
! (expr-size (human Jhon))                ;Result: 2

;Helper function to get maximum of two numbers
(: max (-> Number Number Number))
(= (max $a $b)
   (if (> $a $b)
       $a
       $b))
;; expression function which returns depth of expression
;; depth is defined as length of longest path from root to leaf
(: expr-depth (-> Expression Number))
(= (expr-depth $expr)
  (case $expr
    (
      (($op $arg1 $arg2)
       (+ 1 (max (expr-depth $arg1) (expr-depth $arg2))))
      (($op $arg1)
       (+ 1 (expr-depth $arg1)))
      ($atom 0)
    )
  )
)

! (expr-depth (implies (human Socrates) (mortal Socrates)))  ;Result: 2
! (expr-depth (human Socrates))         ;Result: 1                
! (expr-depth (and (or A B) (and C D)))   ;Result: 3 

;; expression function which checks if an atom is present in the expression
(: expr-contains? (-> Expression Atom Bool))
(= (expr-contains? $expr $target)
  (case $expr
    (
      (($op $arg1 $arg2)
       (or (== $op $target)
           (or (expr-contains? $arg1 $target)
               (expr-contains? $arg2 $target))))
      (($op $arg1)
       (or (== $op $target)
           (expr-contains? $arg1 $target)))
      ($atom (== $atom $target))
    )
  )
)

! (expr-contains? (implies (human Socrates) (mortal Socrates)) Socrates)  ;Result: True
! (expr-contains? (parent Bob Adam) Adam)                                 ;Result: True

;; expression function which substitutes all occurrences of an atom with another atom
(: expr-substitute (-> Expression Atom Atom Expression))
(= (expr-substitute $expr $old $new)
  (case $expr
    (
      (($op $arg1 $arg2)
       ((if (== $op $old) $new $op)
        (expr-substitute $arg1 $old $new)
        (expr-substitute $arg2 $old $new)))
      (($op $arg1)
       ((if (== $op $old) $new $op)
        (expr-substitute $arg1 $old $new)))
      ($atom (if (== $atom $old) $new $atom))
    )
  )
)

! (expr-substitute (implies (human Socrates) (mortal Socrates)) Socrates Plato)
;return: (implies (human Plato) (mortal Plato))
! (expr-substitute (parent John Bob) John "Father")
;return: (parent "Father" Bob)
! (expr-substitute (f (g x) (h y)) y z)
;return: (f (g x) (h z))

;;Helper function for flattening expressions
(: append (-> (List $a) (List $a) (List $a)))
(= (append Nil $ys) $ys)
(= (append (Cons $x $xs) $ys)
   (Cons $x (append $xs $ys)))
;; expression function which flattens an expression into a list of atoms
(: expr-flatten (-> Expression (List Atom)))
(= (expr-flatten $expr)
  (case $expr
    (
      (($op $arg1 $arg2)
       (append (Cons $op Nil)
               (append (expr-flatten $arg1)
                       (expr-flatten $arg2))))
      (($op $arg1)
       (append (Cons $op Nil)
               (expr-flatten $arg1)))
      ($atom (Cons $atom Nil))
    )
  )
)

! (expr-flatten (implies (human Socrates) (mortal Socrates)))
; Returns: [implies, human, Socrates, mortal, Socrates]
! (expr-flatten (and (or A B) (and C D))) 
; Returns: [and, or, A, B, and, C, D]
