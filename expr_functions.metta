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