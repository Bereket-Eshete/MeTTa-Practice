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
           $exp)))

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

(= (getChildren $exp)
   (if (== (get-metatype $exp) Expression)
       (if (or (== (car-atom $exp) AND) (== (car-atom $exp) OR))
           (cdr-atom $exp)
           ())
       ()))

(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))

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
                    ;; Intersection check from GitHub version
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

;; A helper function used to apply andCut in the until function.
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

; ;; Test Case 1: Original working case
; !(andCut (OR C (AND (OR (AND B D) (NOT E)))) (AND (OR (AND B D) (NOT E))))

; ;; Test Case 2: NOT expression case  
; !(andCut (OR X (AND (NOT Y))) (AND (NOT Y)))

; ;; Test Case 3: AND with multiple grandchildren
; !(andCut (AND P Q (AND (OR R S T))) (AND (OR R S T)))

; ;; Test Case 4: Nested AND structures
; !(andCut (OR A (AND (AND (OR B C)))) (AND (AND (OR B C))))

; ;; Test Case 5: Mixed literals and expressions
; !(andCut (AND X Y (AND (OR Z (NOT W)))) (AND (OR Z (NOT W))))

; ;; Test Case 6: Complex nested structure
; !(andCut (OR M (AND (OR (AND N O) (NOT P) Q))) (AND (OR (AND N O) (NOT P) Q)))

; ;; Test Case 7: Case where transformation should NOT apply (AND has guard set)
; !(andCut (OR F (AND G (OR H I))) (AND G (OR H I)))

; ;; Test Case 8: Case where transformation should NOT apply (AND has multiple children)
; !(andCut (AND J (AND (OR K L) (OR M N))) (AND (OR K L) (OR M N)))

!(andCut (OR (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J)))) (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J))))