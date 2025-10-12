;; ===================================================================== ;;
;; IMPROVED EXPRESSION FUNCTIONS                                         ;;
;; Using get-metatype, car-atom, cdr-atom for general expressions       ;;
;; ===================================================================== ;;

;; Helper function to get maximum of two numbers
(: max (-> Number Number Number))
(= (max $a $b)
   (if (> $a $b) $a $b))

;;Helper function to append two lists
(: append (-> (List $a) (List $a) (List $a)))
(= (append Nil $ys) $ys)
(= (append (Cons $x $xs) $ys)
   (Cons $x (append $xs $ys)))

;; ===================================================================== ;;
;; EXPRESSION SIZE FUNCTION                                              ;;
;; ===================================================================== ;;
;; Uses built-in size-atom function - counts elements without nesting
(: expr-size (-> Atom Number))
(= (expr-size $expr)
  (case (get-metatype $expr)
    ((Expression (size-atom $expr))
     ($_ 1))))

!(expr-size (implies (human Socrates) (mortal Socrates)))  ; 3 elements
!(expr-size (f a b c d))                                   ; 5 elements: 
!(expr-size (and A B C D E F))                             ; 7 elements
;; ===================================================================== ;;
;; EXPRESSION DEPTH FUNCTION                                             ;;
;; ===================================================================== ;;
;; Calculates maximum nesting depth using recursive traversal
(: expr-depth (-> Atom Number))
(= (expr-depth $expr)
  (case (get-metatype $expr)
    ((Expression
      (if (== $expr ()) 
          0
          (let* (($head (car-atom $expr))
                 ($tail (cdr-atom $expr))
                 ($head-depth (expr-depth $head))
                 ($tail-depth (expr-depth $tail)))
            (max (+ 1 $head-depth) $tail-depth))))
     ($_ 0))))

!(expr-depth (human Socrates))                             ; Should be 1
!(expr-depth (implies (human Socrates) (mortal Socrates))) ; Should be 2
!(expr-depth human)                                        ; Should be 0

;; ===================================================================== ;;
;; EXPRESSION CONTAINS FUNCTION                                          ;;
;; ===================================================================== ;;
;; Checks if atom appears anywhere in expression
(: expr-contains? (-> Atom Atom Bool))
(= (expr-contains? $expr $target)
  (case (get-metatype $expr)
    ((Expression
      (if (== $expr ()) 
          False
          (let* (($head (car-atom $expr))
                 ($tail (cdr-atom $expr)))
            (or (expr-contains? $head $target)
                (expr-contains? $tail $target)))))
     ($_ (== $expr $target)))))

!(expr-contains? (human Socrates) Socrates)                   ; True
!(expr-contains? (implies (human Socrates) (mortal Socrates)) Socrates)  ; Should be True
!(expr-contains? (f a b c d) c)                               ; True
!(expr-contains? human Socrates)                              ; False
!(expr-contains? Socrates Socrates)                           ; True

;; ===================================================================== ;;
;; EXPRESSION SUBSTITUTE FUNCTION                                        ;;
;; ===================================================================== ;;
;; Replaces all occurrences of atom with another atom 
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
! (expr-substitute (implies (human Socrates) (mortal Socrates)) Socrates Plato) ;(implies (human Plato) (mortal Plato))
! (expr-substitute (parent John Bob) John "Father") ;(parent "Father" Bob)
! (expr-substitute (f (g x) (h y)) h  z) ;(f (g x) (z y))

;; ===================================================================== ;;
;; EXPRESSION FLATTEN FUNCTION                                           ;;
;; ===================================================================== ;;
;; Converts expression to flat list using car-atom/cdr-atom
(: expr-flatten (-> Atom (List Atom)))
(= (expr-flatten $expr)
  (case (get-metatype $expr)
    ((Expression
      (if (== $expr ())
          Nil
          (let* (($head (car-atom $expr))
                 ($tail (cdr-atom $expr)))
            (append (expr-flatten $head)
                    (expr-flatten $tail)))))
     ($_ (Cons $expr Nil)))))


!(expr-flatten (f A B C D E F G H I))   ;[cons f (cons A (cons B (cons C (cons D (cons E (cons F (cons G (cons H (cons I nil)))))))))]
!(expr-flatten (implies (human Socrates) (mortal Socrates))) ;[cons implies (cons human (cons Socrates (cons mortal (cons Socrates nil))))]
!(expr-flatten (and (or A B) (and C D))) 
