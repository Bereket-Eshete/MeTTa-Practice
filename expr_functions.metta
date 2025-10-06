(: expr-size (-> Expression Number))
(= (expr-size $expr)
  (case $expr
    (
      (($op $arg1 $arg2)
       (+ (+ 1 (expr-size $arg1)) (expr-size $arg2)))  ; Nest the additions
      (($op $arg1)
       (+ 1 (expr-size $arg1)))
      ($atom 1)
    )
  )
)

! (expr-size (implies (human Socrates) (mortal Socrates))) 
! (expr-size (human abebe)) ; Should be 5

(: max (-> Number Number Number))
(= (max $a $b)
   (if (> $a $b)
       $a
       $b))

;; Now the expr-depth function should work
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

! (expr-depth (implies (human Socrates) (mortal Socrates)))  ; Should be 2
! (expr-depth (human Socrates))                              ; Should be 1
! (expr-depth (and (or A B) (and C D)))     
! (expr-depth (parent x y))                 ; Should be 2
! (expr-depth (parent x (mother y)))       ; Should be 3


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

;; Test it
! (expr-contains? (implies (human Socrates) (mortal Socrates)) Socrates)  ; Should be True
! (expr-contains? (implies (human Socrates) (mortal Socrates)) Plato)     ; Should be False
! (expr-contains? (and A B) A)                                           ; Should be True
! (expr-contains? human human)    
! (expr-contains? (parent Bob Adam) Adam)                                       ; Should be True