;; =============================================
;; DELETE INCONSISTENT HANDLE - Reduct Rule
;; =============================================
;; Removes AND nodes that create inconsistent literal sets when combined with dominant set
;; 
;; Parameters:
;;   - $parent: The parent expression containing the current node
;;   - $current: The AND node being evaluated (Point of Application)
;;   - $dominantSet: Set of literals from dominant context
;;
;; Returns: (updated-parent updated-current transformation-applied?)
;; =============================================

;; Helper functions

;; Filter to keep only literal atoms (NOT expressions and simple symbols)
(= (filterLiterals $exp)
        (if (== (get-metatype $exp) Symbol)
            (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
            (if (== (car-atom $exp) NOT)
                $exp
                (empty))))

;; Extract all literals from an expression
(= (getLiterals $exp)
     (case $exp
       (
         (() $exp)
         ((NOT $x) $exp)
         ($else (if (== (get-metatype $exp) Expression) (collapse (filterLiterals (superpose $exp))) $exp)))))

;; Extract guard set (literals) from an expression - AND nodes have guard sets, OR nodes do not
(= (getGuardSet $exp)
    (if (== (car-atom $exp) OR)
        ()
        (getLiterals $exp)))

;; Concatenate two tuples
(= (concatTuple $x $y) (union-atom $x $y))

;; Tuple concatenation with type handling
(= (++ $x $y)
   (case ((isSymbol $x) (isSymbol $y))
      (
        ((True True)  ($x $y))
        ((True False) (cons-atom $x $y))
        ((False True) (appendAtom $y $x))
        ((False False)  (foldr cons-atom $y $x)))))

;; Fold right operation for tuples
(: foldr (-> (-> $a $b $b) $b $c $d))
(= (foldr $f $i $xs)
   (if (== $xs ())
       $i
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($ft (foldr $f $i $t)))
         ($f $h $ft))))

;; Check if atom is a symbol
(: isSymbol (-> Expression Bool))
(= (isSymbol $expr) (== (get-metatype $expr) Symbol))

;; Append atom to another atom
(: appendAtom (-> Atom Atom Atom))
(= (appendAtom $a $atom) (foldr cons-atom ($a) $atom))

;; Remove all occurrences of an element from a tuple
;; Fixed removeElement that preserves expression structure
(= (removeElement $common $expr)
   (if (== (get-metatype $expr) Expression)
       ;; For expressions: keep operator, filter children
       (let* (($op (car-atom $expr))
              ($children (cdr-atom $expr))
              ($filteredChildren (removeElementFromTuple $common $children)))
         (if (== $filteredChildren ())
             ()
             (cons-atom $op $filteredChildren)))
       ;; For atoms: use original logic
       (if (== $expr $common) () $expr)))

;; Helper to remove elements from tuples
(= (removeElementFromTuple $common $tuple)
   (if (== $tuple ())
       ()
       (let* (($first (car-atom $tuple))
              ($rest (cdr-atom $tuple))
              ($filteredRest (removeElementFromTuple $common $rest)))
         (if (== $first $common)
             $filteredRest
             (if (== $filteredRest ())
                 ($first)
                 (cons-atom $first $filteredRest))))))
;; Check if an element is member of a list
(: isMember (-> $a $aa Bool))
(= (isMember $x $list)
    (not (== (collapse (intersection (superpose ($x)) (superpose $list))) ()))
)

;; Check if two literals are negations of each other
(= (areNegations ($a $b))
     (or (== $a (NOT $b)) (== $b (NOT $a)))
)

;; Check if a set of literals contains any contradictory pairs (A and NOT A)
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
        (if $isConsistent ($parent $current False) ((removeElement $current $parent) () True))
    )
)

; =============================================
; TEST CASES
; =============================================
!(deleteInconsistentHandle (OR C (AND A B (OR C D))) (AND A B (OR C D)) (D E))