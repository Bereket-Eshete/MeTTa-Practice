(=(length Nil) 0)
(=(length (cons $x $y)) (+ 1 (length $y)))

;lenght of empty list is 0
;!(length Nil) ; lenght []

;$x=a, $y=(cons b Nil)
;(+ 1 (lenght $y))
;$x=b , $y=Nil
;(+ 1 (+ 1 (lenght Nail)))
;(+ 1 (+ 1 0))
;(+ 1 1)
;[2] // result
;!(length (cons a (cons b Nil))) ; lenght [a,b]
;!(length (cons 1 (cons 2 (cons 3 Nil)))); lenght [1,2,3]

;map function
(=(map $f Nil) Nil)
(=(map $f (cons $x $y)) 
        (cons ($f $x) 
            (map $f $y)
          )
        )
;square function
(=(square $n)(* $n $n))
;!(map square (cons 1(cons 2 (cons 3 Nil))))

;foldl
(=(foldl $f $i Nil) $i)
(=(foldl $f $i (cons $x $xs))
    (foldl $f ($f $i $x) $xs))

      ;(foldl + (+ 0 1) (cons 2 Nil))
      ;(foldl + (1) (cons 2 Nil))
      ;(foldl + (+ 1 2) Nil)
      ;(foldl + (3) Nil)
      ;3
!(foldl - 0 (cons 1(cons 2 (cons 3 Nil))))

;foldr
(=(foldr $f $i Nil) $i)
(=(foldr $f $i (cons $x $xs))
    ($f $x (foldr $f $i $xs)))
(= (max $x $y)(
  if (> $x $y) $x $y
))
    ;foldr + 0 [1,2,3]
    ;(+ 1 (foldr + 0 [2,3]))
    ;(+ 1 (+ 2 (foldr + 0 [3])))
    ;(+ 1 (+ 2 (+ 3 (foldr + 0 []))))
    ;(+ 1 (+ 2 (+ 3 0)))
    ;(+ 1 (+ 2 3))
    ;(+ 1 5)
    ;6
!(foldr max 0 (cons 1(cons 2 (cons 8 Nil))))
;revers list function 
(=(reverse Nil) Nil)
(=(reverse (cons $x $xs))
    (append (reverse $xs) (cons $x Nil)))
(=(append Nil $ys) $ys)
(=(append (cons $x $xs) $ys)
    (cons $x (append $xs $ys)))

!(reverse (cons a(cons b Nil)))
;!(reverse (cons 1(cons 2 (cons 3 Nil))))
;[1,2,3]
;append reverse([2,3]) [1]
;append (append reverse([3]) [2]) [1]
;append (append (append reverse([]) [3]) [2]) [1]
;append (append (append [] [3]) [2]) [1]
;append ([3] [2]) [1]
;append [3,2] [1]