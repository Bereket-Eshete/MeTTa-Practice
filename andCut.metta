;; Helper functions
(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

(= (getLiterals $exp)
   (case $exp
     (() $exp)
     ((NOT $x) $exp)
     ($else (if (== (get-metatype $exp) Expression) (collapse (filterLiterals (superpose $exp))) $exp))))

(= (getGuardSet $exp)
   (if (== (car-atom $exp) OR)
       ()
       (getLiterals $exp)))

(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))

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

;; Main andCut function
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