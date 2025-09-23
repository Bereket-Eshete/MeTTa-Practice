(=(length Nil) 0)
(=(length (cons $x $y)) (+ 1 (length $y)))

;lenght of empty list is 0
!(length Nil) ; lenght []

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
!(map square (cons 1(cons 2 (cons 3 Nil))))

;fold-left