;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MeTTa List Functions - Practice Implementation                        ;;
;; A collection of fundamental list manipulation functions               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))
;; LENGTH FUNCTION                                                       ;;
;; Calculates the number of elements in a list
;; Base case: empty list has length 0
(: length (-> (List $a) Number))
(= (length Nil) 0)
;; Recursive case: 1 + length of tail
(= (length (Cons $x $y)) 
   (+ 1 (length $y)))

!(length Nil)                                  ; Result: 0
!(length (Cons 1 (Cons 2 (Cons 3 Nil))))       ; Result: 3


;; MAP FUNCTION                                                         
;; Applies a function to every element in a list
;; Base case: mapping over empty list returns empty list
(: map (-> (-> $a $b) (List $a) (List $b)))
(= (map $f Nil) Nil)
;; Recursive case: apply function to head, map over tail
(= (map $f (Cons $x $y)) 
   (Cons ($f $x) 
         (map $f $y)))

;; Helper function for map examples
(: square (-> Number Number))
(= (square $n) (* $n $n))

!(map square (Cons 1 (Cons 2 (Cons 3 Nil))))   ; Result: [1, 4, 9]
;(Cons (square 1) (map square (Cons 2 (Cons 3 Nil))))
;(Cons 1 (Cons (square 2) (map sqaure (Cons 3 Nil))))
;(Cons 1 (Cons 4 (Cons (square 3) (map square Nil))))
;(Cons 1 (Cons 4 (Cons 9 Nil)))

;; FOLD LEFT FUNCTION                                                    ;;
;; Reduces a list from left to right using a binary function
;; Base case: folding empty list returns initial value
(: foldl (-> (-> $acc $elem $acc) $acc (List $a) $acc))
(= (foldl $f $i Nil) $i)
(= (foldl $f $i (Cons $x $xs))
   (foldl $f ($f $i $x) $xs))
!(foldl + 0 Nil)                              ; Result: 0
!(foldl + 0 (Cons 1 (Cons 2 (Cons 3 Nil))))   ; Result: 6
;(foldl + (+ 0 1) (Cons 2 (Cons 3 Nil)))
;(foldl + (+ 1 2) (Cons 3 Nil))
;(foldl + (+ 3 3) Nil)
;(foldl + 6 Nil)
;6 


;; FOLD RIGHT FUNCTION                                                   ;;
;; Reduces a list from right to left using a binary function
;; Base case: folding empty list returns initial value
(: foldr (-> (-> $elem $acc $acc) $acc (List $a) $acc))
(= (foldr $f $i Nil) $i)
;; Recursive case: apply function to head and result of folding tail
(= (foldr $f $i (Cons $x $xs))
   ($f $x (foldr $f $i $xs)))

;; Helper function for foldr examples
(: map (-> Number Number Number))
(= (max $x $y)
   (if (> $x $y) $x $y))


!(foldr + 0 (Cons 1 (Cons 8 (Cons 3 Nil))))  ; Result: 8
!(foldr max 0 (Cons 1 (Cons 8 (Cons 3 Nil)))) ; Result: 8


;; REVERSE FUNCTION                                                      ;;
;; Reverses the order of elements in a list
;; Base case: reversing empty list returns empty list
(: reverse (-> (List $a) (List $a)))
(= (reverse Nil) Nil)
;; Recursive case: reverse tail and append head at the end
(= (reverse (Cons $x $xs))
   (append (reverse $xs) (Cons $x Nil)))
;; Helper function for appending lists
;; Concatenates two lists together
;; Base case: appending to empty list returns second list
(: append (-> (List $a) (List $a) (List $a)))
(= (append Nil $ys) $ys)
;; Recursive case: prepend head of first list, append tail to second list
(= (append (Cons $x $xs) $ys)
   (Cons $x (append $xs $ys)))


 !(reverse (Cons 1 (Cons 2 (Cons 3 Nil))))      ; Result: [3, 2, 1]
 !(append (Cons 1 (Cons 2 Nil)) (Cons 3 Nil))   ; Result: [1, 2, 3]





;; FILTER FUNCTION                                                       ;;
;; Filters elements that satisfy a predicate function
(: filter (-> (-> $a Bool) (List $a) (List $a)))
(= (filter $p Nil) Nil)
(= (filter $p (Cons $x $xs))
   (if ($p $x)
       (Cons $x (filter $p $xs))
       (filter $p $xs)))

;; Helper predicate function
(: is-positive (-> Number Bool))
(= (is-positive $n) (> $n 0))

!(filter is-positive (Cons -1 (Cons 2 (Cons -3 (Cons 4 Nil)))))  ; Result: [2, 4]

;; Basic list operations
(: head (-> (List $a) $a))
(= (head (Cons $x $xs)) $x)

(: tail (-> (List $a) (List $a)))
(= (tail (Cons $x $xs)) $xs)

;; Take function - deterministic version
(: take (-> Number (List $a) (List $a)))
(= (take $n $xs)
   (if (== $n 0) 
       Nil
       (if (== $xs Nil)
           Nil
           (Cons (head $xs) (take (- $n 1) (tail $xs))))))

;; Test it!
! (take 2 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
; Should return ONLY: (Cons 1 (Cons 2 Nil))

;; MEMBER FUNCTION                                                       
;; Checks if an element exists in a list
(: member (-> $a (List $a) Bool))
(= (member $x Nil) False)
(= (member $x (Cons $y $ys))
   (if (== $x $y)
       True
       (member $x $ys)))

!(member 1 Nil)
!(member 3 (Cons 1 (Cons 2 (Cons 3 Nil))))                     ; Result: True
!(member 5 (Cons 1 (Cons 2 (Cons 3 Nil))))                     ; Result: False


;; Renamed functions for clarity
(: unique-list (-> (List $a) (List $a)))
(= (unique-list $list)
   (reverse (build-unique $list Nil)))  ; ← ADDED REVERSE HERE

(: build-unique (-> (List $a) (List $a) (List $a)))
(= (build-unique Nil $unique-so-far) $unique-so-far)
(= (build-unique (Cons $current $remaining) $unique-so-far)
   (if (member $current $unique-so-far)
       (build-unique $remaining $unique-so-far)          
       (build-unique $remaining (Cons $current $unique-so-far)))) 


! (unique-list (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 1 (Cons 4 Nil)))))))
