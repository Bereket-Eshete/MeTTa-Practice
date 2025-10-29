;; =============================================
;; GENERAL HELPERS (from metta-moses:utilities:general-helpers)
;; =============================================

(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

(= (concatTuple $x $y) (union-atom $x $y))

(: ++ (-> Expression Expression Expression))
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

;; =============================================
;; RTE HELPERS (from metta-moses:reduct:boolean-reduct:rte-helpers)
;; =============================================

(= (filterLiterals $exp)
   (if (== (get-metatype $exp) Symbol)
       (if (or (== $exp AND) (== $exp OR)) (empty) $exp)
       (if (== (car-atom $exp) NOT)
           $exp
           (empty))))

(= (getGuardSet $exp)
   (if (== (car-atom $exp) OR)
       ()
       (getLiterals $exp)))

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

(= (filterChildren $exp)
   (if (== (get-metatype $exp) Symbol)
       (empty)
       (if (== (car-atom $exp) NOT)
           (empty)
           $exp)))

;; Fixed getLiterals using the correct get-metatype pattern
(= (getLiterals $exp)
   (case (get-metatype $exp)
     ((Expression
       (case $exp
         (() ())
         ((NOT $x) $exp)
         (($op . $rest) ())  ; AND/OR nodes have no literals
         ($else $exp)))
     (Symbol 
      (if (or (== $exp AND) (== $exp OR)) 
          () 
          $exp))
     ($else $exp))))

;; Fixed getChildren using the correct get-metatype pattern  
(= (getChildren $exp)
   (case (get-metatype $exp)
     ((Expression
       (case $exp
         (() ())
         ((NOT $x) ())
         (($op . $rest) $rest)  ; Extract children from AND/OR nodes
         ($else ())))
     ($else ()))))

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



(= (getLiteralChildren $expr) 
   ((getLiterals $expr) (getChildren $expr)))

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

;; =============================================
;; ORCUT FUNCTION (from metta-moses:reduct:boolean-reduct:cut-unnecessary-or)
;; =============================================

;; A function to return the literals and children of any given node.
(: getSubExpression (-> Expression Expression))
(= (getSubExpression $exp)
   (case $exp
     (
        ((NOT $a) ())
        ($else
          (let*
            (
              ($literals (getLiterals $exp))
              ($children (getChildren $exp))
            )
            (concatTuple $literals $children)
          )
        )
     )
   )
)

;; A function to remove an unnecessary OR and its child.
(: orCut (-> Expression Expression (Expression Expression Bool)))
(= (orCut $parent $current)
   (case $current
     (
       ((OR (NOT $x)) ((addLiterals (removeElement ($current) $parent) ((NOT $x))) () True))
       ((OR $x) (if (== (get-metatype $x) Symbol)
                     ((addLiterals (removeElement ($current) $parent) ($x)) () True)
                     ((addChildren (removeElement ($current) $parent) (getSubExpression $x)) () True)
                   )
       )
       ($else ($parent $current False))
     )
   )
)

;; A helper function used to apply orCut function in the until function.
(: applyOrCut (-> (Expression Bool (Expression Expression)) (Expression Bool (Expression Expression))))
(= (applyOrCut ($parent $bool ($child $remainingChildren)))
      (let*
        (
          (($updatedParent $updatedChild $appliedOrCut) (orCut $parent $child))
          ($nextChild (if (~= $remainingChildren ()) (car-atom $remainingChildren) ()))
          ($restChildren (if (~= $remainingChildren ()) (cdr-atom $remainingChildren) ()))
        )
        ($updatedParent (or $appliedOrCut $bool) ($nextChild $restChildren))
      )
)

;!(orCut (AND B (OR (AND (NOT D)))) (OR (AND (NOT D))))

