;; =============================================================================
;; andCut - Reduct Rule
;; =============================================================================
;; Eliminates redundant AND nodes that serve no logical purpose when they:
;;   - Contain no guard literals (empty guard set)
;;   - Have exactly one child expression
;;   - All grandchildren have mutually exclusive guard sets (no common literals)
;;
;; Transformation:
;;   Removes the unnecessary AND node and promotes its grandchildren directly
;;   to the parent level, preserving the logical meaning while simplifying
;;   the expression structure.
;;
;; Parameters:
;;   $parent  - The parent expression containing the AND node to evaluate
;;   $current - The AND node being considered for elimination (Point of Application)
;;
;; Returns: (updated-parent () transformation-applied?)
;;   - On success: (simplified-parent () True)
;;   - On failure: (original-parent original-current False)
;;

;; Helper functions for andCut

;; Basic utilities
(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

;; Filter functions for literals and children
(= (filterLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
       (if (== (car-atom $exp) NOT)
           $exp
           $exp)))
;; filterChildren returns cdr-atom to get all children
(= (filterChildren $exp)
   (if (== (get-metatype $exp) Symbol)
       (empty)
       (if (== (car-atom $exp) NOT)
           (empty)
           (cdr-atom $exp))))

;; Core expression analysis functions
(= (getLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) 
           () 
           $exp)
       (if (or (== (car-atom $exp) AND) (== (car-atom $exp) OR))
           ()
           $exp)))
;; returns cdr-atom to get all children
(= (getChildren $exp)
   (if (== (get-metatype $exp) Expression)
       (if (or (== (car-atom $exp) AND) (== (car-atom $exp) OR))
           (cdr-atom $exp)
           ())
       ()))
;; Get both literals and children as a tuple
(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))
;; Get guard set (literals) from an expression
(= (getGuardSet $exp)
   (if (== (get-metatype $exp) Expression)
       (if (== (car-atom $exp) OR)
           ()
           (getLiterals $exp))
       (if (== (get-metatype $exp) Symbol)
           $exp
           ())))

;; Helper to get guard sets from a tuple of children
(= (getGuardSets $children)
   (if (== $children ()) 
       ()
       (let* (($first (car-atom $children))
              ($rest (cdr-atom $children))
              ($firstGuard (getGuardSet $first))
              ($restGuards (getGuardSets $rest)))
         (if (== (get-metatype $firstGuard) Expression)
             (concatTuple $firstGuard $restGuards)
             (if (== $restGuards ())
                 ($firstGuard)
                 (cons-atom $firstGuard $restGuards))))))

;; Tuple manipulation functions
(= (removeElement $common $expr)
   (if (== (get-metatype $expr) Expression)
       (let* (($op (car-atom $expr))
              ($children (cdr-atom $expr))
              ($filtered (removeElementFromTuple $common $children)))
         (if (== $filtered ())
             ()
             (cons-atom $op $filtered)))
       (if (== $expr $common) () $expr)))
;; Remove all occurrences of an element from a tuple
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
;; Helpers to add children and literals back to an expression
(= (addChildren $exp $children)
   (if (== $children ())
       $exp
       (let*
         (
           ($op (car-atom $exp))
           ($oldChildren (getChildren $exp))
           ($newChildren (if (== $oldChildren ())
                             $children
                             (concatTuple $oldChildren $children)))
           ($updatedSubExpression (if (== (getLiterals $exp) ())
                                      $newChildren
                                      (concatTuple (getLiterals $exp) $newChildren)))
         )
         (cons-atom $op $updatedSubExpression)
       )))
;; Helper to add literals back to an expression
(= (addLiterals $exp $elements)
   (if (== $elements ())
       $exp
       (let*
         (
           ($op (car-atom $exp))
           ($guardSet (getLiterals $exp))
           ($newGuardSet (if (== $guardSet ())
                             (if (== (get-metatype $elements) Expression)
                                 ($elements)
                                 $elements)
                             (if (== (get-metatype $elements) Expression)
                                 (concatTuple $guardSet ($elements))
                                 (concatTuple $guardSet $elements))))
           ($children (getChildren $exp))
           ($updatedSubExpression (if (== $children ())
                                      $newGuardSet
                                      (concatTuple $newGuardSet $children)))
         )
         (cons-atom $op $updatedSubExpression)
       )))
;; concatenate two tuples
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

;; Additional helpers
(: isSymbol (-> Expression Bool))
(= (isSymbol $expr) (== (get-metatype $expr) Symbol))

;; Main andCut function with intersection check
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
                    ;; Properly extract literals and children from the tuple
                    ($grandTuple (getLiteralChildren (car-atom $child)))
                    ($grandLiteralsTemp (car-atom $grandTuple))
                    ($grandLiterals (if (and (== (get-metatype $grandLiteralsTemp) Expression)
                                             (not (== $grandLiteralsTemp ())))
                                        $grandLiteralsTemp
                                        $grandLiteralsTemp))
                    ($grandChildrenTemp (cdr-atom $grandTuple))
                    ($grandChildren (if (and (== (get-metatype $grandChildrenTemp) Expression)
                                             (not (== $grandChildrenTemp ())))
                                        (car-atom $grandChildrenTemp)
                                        $grandChildrenTemp))
                    ;; Find common literals among grandChildren
                    ($allLiterals (getGuardSets $grandChildren))
                    ($common (intersections $allLiterals))
                )
                (if (== $common ())
                    (let*
                        (
                            ($updatedParent (removeElement $current $parent))
                            ($updatedParent' (addChildren $updatedParent $grandChildren))
                            ($updatedParent'' (addLiterals $updatedParent' $grandLiterals))
                        )
                        ($updatedParent'' () True)
                    )
                    ($parent $current False)
                )
            )
            ($parent $current False)
        )
    )
)
;; A Helper to apply andCut over a parent expression with multiple children
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

;; TEST CASES
!(andCut (OR A (AND B (OR C D))) (AND B (OR C D)))
!(andCut (OR A (AND (OR (AND B C) (AND E F) (AND G H))) F) (AND (OR (AND B C) (AND E F) (AND G H))))
