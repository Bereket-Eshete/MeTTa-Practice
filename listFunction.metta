;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MeTTa List Functions - Practice Implementation                        ;;
;; A collection of fundamental list manipulation functions                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; LENGTH FUNCTION                                                       ;;
;; Calculates the number of elements in a list
;; Base case: empty list has length 0
(= (length Nil) 0)
;; Recursive case: 1 + length of tail
(= (length (cons $x $y)) 
   (+ 1 (length $y)))

!(length Nil)                                  ; Result: 0
!(length (cons 1 (cons 2 (cons 3 Nil))))       ; Result: 3


;; MAP FUNCTION                                                         
;; Applies a function to every element in a list
;; Base case: mapping over empty list returns empty list
(= (map $f Nil) Nil)
;; Recursive case: apply function to head, map over tail
(= (map $f (cons $x $y)) 
   (cons ($f $x) 
         (map $f $y)))

;; Helper function for map examples
(= (square $n) (* $n $n))

!(map square (cons 1 (cons 2 (cons 3 Nil))))   ; Result: [1, 4, 9]


;; FOLD LEFT FUNCTION                                                    ;;
;; Reduces a list from left to right using a binary function
;; Base case: folding empty list returns initial value
(= (foldl $f $i Nil) $i)
;; Recursive case: apply function to accumulator and head, continue with tail
(= (foldl $f $i (cons $x $xs))
   (foldl $f ($f $i $x) $xs))

!(foldl + 0 (cons 1 (cons 2 (cons 3 Nil))))   ; Result: 6


;; FOLD RIGHT FUNCTION                                                   ;;
;; Reduces a list from right to left using a binary function
;; Base case: folding empty list returns initial value
(= (foldr $f $i Nil) $i)
;; Recursive case: apply function to head and result of folding tail
(= (foldr $f $i (cons $x $xs))
   ($f $x (foldr $f $i $xs)))

;; Helper function for foldr examples
(= (max $x $y)
   (if (> $x $y) $x $y))


!(foldr max 0 (cons 1 (cons 8 (cons 3 Nil))))  ; Result: 8


;; REVERSE FUNCTION                                                      ;;
;; Reverses the order of elements in a list
;; Base case: reversing empty list returns empty list
(= (reverse Nil) Nil)
;; Recursive case: reverse tail and append head at the end
(= (reverse (cons $x $xs))
   (append (reverse $xs) (cons $x Nil)))
;; Helper function for appending lists
;; Concatenates two lists together
;; Base case: appending to empty list returns second list
(= (append Nil $ys) $ys)
;; Recursive case: prepend head of first list, append tail to second list
(= (append (cons $x $xs) $ys)
   (cons $x (append $xs $ys)))


 !(reverse (cons 1 (cons 2 (cons 3 Nil))))      ; Result: [3, 2, 1]
 !(append (cons 1 (cons 2 Nil)) (cons 3 Nil))   ; Result: [1, 2, 3]


;; UNIQUE FUNCTION                                                       ;;
;; Removes duplicate elements from a list, keeping first occurrence
;; Type signature for proper MeTTa List type
(: unique (-> (List $a) (List $a)))
;; Base case: empty list has no duplicates
(= (unique Nil) Nil)
;; Recursive case: if head exists in tail, skip it; otherwise keep it
(= (unique (Cons $x $xs))
   (if (List.contains $x $xs)
       (unique $xs)
       (Cons $x (unique $xs))))


;;!(unique (Cons 1 (Cons 2 (Cons 1 (Cons 3 (Cons 2 Nil))))))  ; Result: [1, 2, 3]
;; !(unique (Cons a (Cons b (Cons a (Cons c Nil)))))            ; Result: [a, b, c]


;; FILTER FUNCTION                                                       ;;
;; Filters elements that satisfy a predicate function
(= (filter $p Nil) Nil)
(= (filter $p (cons $x $xs))
   (if ($p $x)
       (cons $x (filter $p $xs))
       (filter $p $xs)))

;; Helper predicate function
(= (is-positive $n) (> $n 0))

!(filter is-positive (cons -1 (cons 2 (cons -3 (cons 4 Nil)))))  ; Result: [2, 4]


;; TAKE FUNCTION                                                         ;;
;; Takes the first n elements from a list
(= (take 0 $xs) Nil)
(= (take $n Nil) Nil)
(= (take $n (cons $x $xs))
   (cons $x (take (- $n 1) $xs)))

!(take 2 (cons 1 (cons 2 (cons 3 (cons 4 Nil)))))              ; Result: [1, 2]


;; MEMBER FUNCTION                                                       
;; Checks if an element exists in a list
(= (member $x Nil) False)
(= (member $x (cons $y $ys))
   (if (== $x $y)
       True
       (member $x $ys)))

!(member 3 (cons 1 (cons 2 (cons 3 Nil))))                     ; Result: True
!(member 5 (cons 1 (cons 2 (cons 3 Nil))))                     ; Result: False