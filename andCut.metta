;; A function to remove an unnecessary AND and its child.
;; Definitions: A guard set of a POA is the set of literals in the POA.
;; Preconditions: The point of application (POA) is and AND node,
;;                has an empty guard set, one child and
;;                for each child C of the POA, the intersection
;;                of the guard sets of the children of C is empty.
;; Action: If the preconditions are met, the function adds the
;;            grandChildren of the POA to the POA's parent and returns
;;            the updated parent, empty atom and a boolean value of
;;            true.
;;         If conditions are not met, the function returns the unchanged state.
;;            updated POA and also a boolean value corresponding to
;;            whether the transformation function is applied or not.
;; Example: input: POA ==> (AND (OR (AND B D) (NOT E))), parent ==> (OR C (AND (OR (AND B D) (NOT E))))
;;          output: ((OR C (AND B D) (NOT E)) () True)

;; Helper functions for andCut

;; Basic utilities
(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

;; Filter functions
(= (filterLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
       (if (== (car-atom $exp) NOT)
           $exp
           (empty))))

(= (filterChildren $exp)
   (if (== (get-metatype $exp) Symbol)
       (empty)
       (if (== (car-atom $exp) NOT)
           (empty)
           $exp)))

;; Core expression analysis functions
(= (getLiterals $exp)
   (case $exp
     (() $exp)
     ((NOT $x) $exp)
     ($else (if (== (get-metatype $exp) Expression) (collapse (filterLiterals (superpose $exp))) $exp))))

(= (getChildren $exp)
   (case $exp
     (() $exp)
     ((NOT $x) ())
     ($else (if (== (get-metatype $exp) Expression) (collapse (filterChildren (superpose $exp))) ()))))

(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))

(= (getGuardSet $exp)
   (if (== (car-atom $exp) OR)
       ()
       (getLiterals $exp)))

;; Tuple manipulation functions
(= (removeElement $common $tuple)
   (collapse (subtraction (superpose $tuple) (superpose $common))))

(= (addChildren $exp $children)
   (let*
     (
        ($op (car-atom $exp))
        ($oldChildren (getChildren $exp))
        ($newChildren (concatTuple $oldChildren $children))
        ($updatedSubExpression (concatTuple (getLiterals $exp) $newChildren))
        ($newExp (cons-atom $op $updatedSubExpression))
     )
     $newExp
   ))

(= (addLiterals $exp $elements)
   (let*
     (
        ($op (car-atom $exp))
        ($guardSet (getLiterals $exp))
        ($newGuardSet (concatTuple $guardSet $elements))
        ($newGuardSet' (collapse (unique (superpose $newGuardSet))))
        ($updatedSubExpression (concatTuple $newGuardSet' (getChildren $exp)))
        ($newExp (cons-atom $op $updatedSubExpression))
     )
     $newExp
   ))

(= (concatTuple $x $y) (union-atom $x $y))

;; Common literal finding
(= (findCommonLiterals $tuple $nestedTuple)
   (if (or (== $nestedTuple ()) (== $tuple ()))
       ()
       (let*
         (
           ($first (car-atom $nestedTuple))
           ($tail (cdr-atom $nestedTuple))
           ($common (collapse (intersection (superpose $tuple) (superpose $first))))
         )
         (if (== $tail ())
             $common
             (findCommonLiterals $common $tail)))))

;; Additional helpers
(: isSymbol (-> Expression Bool))
(= (isSymbol $expr) (== (get-metatype $expr) Symbol))

(: foldr (-> (-> $a $b $b) $b $c $d))
(= (foldr $f $i $xs)
   (if (== $xs ())
       $i
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($ft (foldr $f $i $t)))
         ($f $h $ft))))

(: appendAtom (-> Atom Atom Atom))
(= (appendAtom $a $atom) (foldr cons-atom ($a) $atom))

(: ++ (-> Expression Expression Expression))
(= (++ $x $y)
   (case ((isSymbol $x) (isSymbol $y))
      (
        ((True True)  ($x $y))
        ((True False) (cons-atom $x $y))
        ((False True) (appendAtom $y $x))
        ((False False)  (foldr cons-atom $y $x)))))

(: andCut (-> Expression Expression (Expression Expression Bool)))
(= (andCut $parent $current)
    (let*
        (
            (($guardSet $child) (getLiteralChildren $current))
            ($tail (cdr-atom $child))
        )
        (if (and (== $tail ()) (== $guardSet ())) ;; Has no guardSet and has single child
            (let*
                (
                    (($grandLiterals $grandChildren) (getLiteralChildren (car-atom $child)))
                    ($allLiterals (collapse (getGuardSet (superpose $grandChildren))))
                    ($common (intersections $allLiterals))
                    ($updatedParent (removeElement ($current) $parent))
                    ($updatedParent' (addChildren $updatedParent $grandChildren))
                    ($updatedParent'' (addLiterals $updatedParent' $grandLiterals))
                )
                (if (== $common ())
                    ($updatedParent'' () True)
                    ($parent $current False)
                )
            )
            ($parent $current False)
        )
    )
)

;; Function to find intersections between all literals in a nested tuple
(: intersections (-> Expression Expression))
(= (intersections $nestedTuple)
   (case $nestedTuple 
     ((() $nestedTuple)
      ($else
        (if (== (cdr-atom $nestedTuple) ())
            ()
            (let*
              (
                ($first (car-atom $nestedTuple))
                ($tail (cdr-atom $nestedTuple)))
              (findCommonLiterals $first $tail)))))))


;; A helper function used to apply andCut in the until function.
;; This function keeps the state of the until function updated for a single function application.
(: applyAndCut (-> (Expression Bool (Expression Expression)) (Expression Bool (Expression Expression))))
(= (applyAndCut ($parent $bool ($child $remainingChildren)))
      (let*
        (
          (($updatedParent $updatedChild $appliedOrCut) (andCut $parent $child))
          ($nextChild (if (~= $remainingChildren ()) (car-atom $remainingChildren) ()))
          ($restChildren (if (~= $remainingChildren ()) (cdr-atom $remainingChildren) ()))
        )
        ($updatedParent (or $appliedOrCut $bool) ($nextChild $restChildren))
      )
)

!(andCut (OR C (AND (OR (AND B D) (NOT E)))) (AND (OR (AND B D) (NOT E))))
