;; A function to remove an unnecessary AND and its child.

;; Helper functions for andCut

;; Basic utilities
(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

;; Filter functions
; (= (filterLiterals $exp)
;    (if (== (get-metatype $exp) Symbol)
;        (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
;        (if (== (car-atom $exp) NOT)
;            $exp
;            (empty))))

;; Fixed filterLiterals function
;; Simple fixed filterLiterals
(= (filterLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
       (if (== (car-atom $exp) NOT)
           $exp
           $exp)))  ; ← Just return the expression instead of (empty)
;; Use the working filterChildren function that returns correct result
(= (filterChildren $exp)
   (if (== (get-metatype $exp) Symbol)
       (empty)
       (if (== (car-atom $exp) NOT)
           (empty)
           (cdr-atom $exp))))

;; Core expression analysis functions
;; General getLiterals function that works for any expression
(= (getLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) 
           () 
           $exp)
       (if (or (== (car-atom $exp) AND) (== (car-atom $exp) OR))
           ()
           $exp)))

;; Use simple getChildren that directly returns cdr-atom for expressions
;; General getChildren function
;; Fixed getChildren - return proper values
;; Fixed getChildren - NOT expressions have no children
(= (getChildren $exp)
   (if (== (get-metatype $exp) Expression)
       (if (== (car-atom $exp) NOT)
           ()  ; NOT expressions have no children
           (cdr-atom $exp))
       ()))
;; Return empty for non-expressions
(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))

;; Helper to get guard sets from a tuple of children
;; Original getGuardSet function (for single expressions)
;; Fixed getGuardSet to handle simple symbols
(= (getGuardSet $exp)
   (if (== (get-metatype $exp) Expression)
       (if (== (car-atom $exp) OR)
           ()
           (getLiterals $exp))
       (if (== (get-metatype $exp) Symbol)
           $exp  ; Simple symbols are their own guard sets
           ())))

;; Helper getGuardSets function (for tuples of expressions)  
;; Fixed getGuardSets to always return tuples
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
;; General removeElement function that works for any expression

;; Fixed addChildren function
(= (addChildren $exp $children)
   (let*
     (
        ($op (car-atom $exp))
        ($oldChildren (getChildren $exp))
        ;; Filter out empty tuples before concatenating
        ($filteredOld (if (== $oldChildren ()) () $oldChildren))
        ($filteredNew (if (== $children ()) () $children))
        ($newChildren (concatTuple $filteredOld $filteredNew))
        ($updatedSubExpression (concatTuple (getLiterals $exp) $newChildren))
        ($newExp (cons-atom $op $updatedSubExpression))
     )
     $newExp
   ))

;; Fixed addLiterals function
;; Fixed addLiterals function
;; Fixed addLiterals function
(= (addLiterals $exp $elements)
   (if (== $elements ())
       $exp
       (let*
         (
           ($op (car-atom $exp))
           ($guardSet (getLiterals $exp))
           ;; Handle the case where we're adding a single expression
           ($newGuardSet (if (== (get-metatype $elements) Expression)
                             ;; Single expression - add it directly
                             (if (== $guardSet ())
                                 ($elements)  ; Wrap in tuple to preserve structure
                                 (cons-atom $elements $guardSet))
                             ;; Multiple elements - use concatTuple
                             (if (== $guardSet ())
                                 $elements
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
;; General removeElement function with proper empty handling
;; General removeElement that works for any expression size
(= (removeElement $common $expr)
   (if (== (get-metatype $expr) Expression)
       ;; For expressions: recursively filter children while preserving structure
       (let* (($op (car-atom $expr))
              ($children (cdr-atom $expr))
              ($filtered (removeElementFromTuple $common $children)))
         (if (== $filtered ())
             ()
             (cons-atom $op $filtered)))
       ;; For atoms: simple comparison
       (if (== $expr $common) () $expr)))

;; Helper function to remove from tuples
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
;; Main andCut function using built-in intersection
(: andCut (-> Expression Expression (Expression Expression Bool)))
;;A helper function used to apply andCut in the until function.
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
                    ;; Safe extraction of literals
                    ($grandLiteralsTemp (car-atom $grandTuple))
                    ($grandLiterals (if (and (== (get-metatype $grandLiteralsTemp) Expression)
                                             (not (== $grandLiteralsTemp ())))
                                        $grandLiteralsTemp
                                        $grandLiteralsTemp))
                    ;; Safe extraction of grandchildren
                    ($grandChildrenTemp (cdr-atom $grandTuple))
                    ($grandChildren (if (and (== (get-metatype $grandChildrenTemp) Expression)
                                             (not (== $grandChildrenTemp ())))
                                        (car-atom $grandChildrenTemp)
                                        $grandChildrenTemp))
                )
                ;; Always apply transformation when preconditions are met
                (let*
                    (
                        ($updatedParent (removeElement $current $parent))
                        ($updatedParent' (addChildren $updatedParent $grandChildren))
                        ($updatedParent'' (addLiterals $updatedParent' $grandLiterals))
                    )
                    ($updatedParent'' () True)
                )
            )
            ($parent $current False)
        )
    )
)
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


;; Test Case 1: Original working case
!(andCut (OR C (AND (OR (AND B D) (NOT E)))) (AND (OR (AND B D) (NOT E))))

;; Test Case 2: NOT expression case  
!(andCut (OR X (AND (NOT Y))) (AND (NOT Y)))

;; Test Case 3: AND with multiple grandchildren
!(andCut (AND P Q (AND (OR R S T))) (AND (OR R S T)))

;; Test Case 4: Nested AND structures
!(andCut (OR A (AND (AND (OR B C)))) (AND (AND (OR B C))))

;; Test Case 5: Mixed literals and expressions
!(andCut (AND X Y (AND (OR Z (NOT W)))) (AND (OR Z (NOT W))))

;; Test Case 6: Complex nested structure
!(andCut (OR M (AND (OR (AND N O) (NOT P) Q))) (AND (OR (AND N O) (NOT P) Q)))

;; Test Case 7: Case where transformation should NOT apply (AND has guard set)
!(andCut (OR F (AND G (OR H I))) (AND G (OR H I)))

;; Test Case 8: Case where transformation should NOT apply (AND has multiple children)
!(andCut (AND J (AND (OR K L) (OR M N))) (AND (OR K L) (OR M N)))

;; Test Case 9: Simple symbol grandchildren
!(andCut (OR U (AND (OR V W))) (AND (OR V W)))

;; Test Case 10: Deep nesting
!(andCut (AND Alpha (AND (OR Beta (AND Gamma Delta)))) (AND (OR Beta (AND Gamma Delta))))

