;; Complete deleteInconsistentHandle implementation with all helper functions

;; Helper functions
(= (filterLiterals $exp)
        (if (== (get-metatype $exp) Symbol)
            (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
            (if (== (car-atom $exp) NOT)
                $exp
                (empty))))

(= (getLiterals $exp)
     (case $exp
       (
         (() $exp)
         ((NOT $x) $exp)
         ($else (if (== (get-metatype $exp) Expression) (collapse (filterLiterals (superpose $exp))) $exp)))))

(= (getGuardSet $exp)
    (if (== (car-atom $exp) OR)
        ()
        (getLiterals $exp)))

(= (concatTuple $x $y) (union-atom $x $y))

(= (++ $x $y)
   (case ((isSymbol $x) (isSymbol $y))
      (
        ((True True)  ($x $y))
        ((True False) (cons-atom $x $y))
        ((False True) (appendAtom $y $x))
        ((False False)  (foldr cons-atom $y $x)))))

(: foldr (-> (-> $a $b $b) $b $c $d))
(= (foldr $f $i $xs)
   (if (== $xs ())
       $i
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($ft (foldr $f $i $t)))
         ($f $h $ft))))

(: isSymbol (-> Expression Bool))
(= (isSymbol $expr) (== (get-metatype $expr) Symbol))

(: appendAtom (-> Atom Atom Atom))
(= (appendAtom $a $atom) (foldr cons-atom ($a) $atom))

(= (removeElement $common $tuple)
   (collapse (subtraction (superpose $tuple) (superpose $common))))

(: isMember (-> $a $aa Bool))
(= (isMember $x $list)
    (not (== (collapse (intersection (superpose ($x)) (superpose $list))) ()))
)

(= (areNegations ($a $b))
     (or (== $a (NOT $b)) (== $b (NOT $a)))
)

(= (isConsistentExp $handleSet)
    (if (== $handleSet ())
        True
        (let*
          (
            ($pairs (collapse ((superpose $handleSet) (superpose $handleSet))))
            ($result (collapse (areNegations (superpose $pairs)))))
          (if (isMember True $result)
              False
              True
          )
        )
    )
)

;; Main deleteInconsistentHandle function
(= (deleteInconsistentHandle $parent $current $dominantSet)
    (let*
        (
          ($guardSet (getGuardSet $current))
          ($handleSet (concatTuple $dominantSet $guardSet))
          ($isConsistent (isConsistentExp $handleSet))
        )
        (if $isConsistent ($parent $current False) ((removeElement ($current) $parent) () True))
    )
)

; Test the complete implementation
!(deleteInconsistentHandle (OR B (AND (NOT A) (OR B C))) (AND (NOT A) (OR B C)) (A))
; Test Case 1: Consistent handle - should NOT apply
!(deleteInconsistentHandle (OR B (AND A (OR B C))) (AND A (OR B C)) (B))

; Test Case 2: Inconsistent handle - should apply  
!(deleteInconsistentHandle (OR C (AND (NOT B) (OR C D))) (AND (NOT B) (OR C D)) (B))

; Test Case 3: Empty dominant set, consistent - should NOT apply
!(deleteInconsistentHandle (OR D (AND E (OR D F))) (AND E (OR D F)) ())

; Test Case 4: Complex inconsistent case
!(deleteInconsistentHandle (OR X (AND (NOT Y) Z (OR X W))) (AND (NOT Y) Z (OR X W)) (Y))
