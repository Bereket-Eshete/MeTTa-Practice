!(register-module! ../Reduct-Rules)
!(import! &self Reduct-Rules:andCut)

;; Test 01: Simple AND elimination with OR grandchildren
!(assertEqual
    (andCut (OR A B (AND (OR (AND B C) (AND E F)))) (AND (OR (AND B C) (AND E F))))
    ((OR A B (AND B C) (AND E F)) () True))
;; expected Result [((OR A B (AND B C) (AND E F)) () True)]

;; Test 02: AND elimination with NOT expressions
!(assertEqual
    (andCut (OR A (AND (OR (AND B C) (AND (NOT E) F)))) (AND (OR (AND B C) (AND (NOT E) F))))
    ((OR A (AND B C) (AND (NOT E) F)) () True))
;; expected Result [((OR A (AND B C) (AND (NOT E) F)) () True)]

;; Test 03: Multiple grandchildren elimination
!(assertEqual
    (andCut (OR A (AND (OR (AND B C) (AND E F) (AND G H))) F) (AND (OR (AND B C) (AND E F) (AND G H))))
    ((OR A F (AND B C) (AND E F) (AND G H)) () True))
;; expected Result [((OR A F (AND B C) (AND E F) (AND G H)) () True)]

;; Test 04: AND with multiple children (should not transform)
!(assertEqual
    (andCut (OR (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J)))) (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J))))
    ((OR (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J)))) (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J))) False))
;; expected Result [((OR (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J)))) (AND (OR (AND B C) (AND E F)) (OR (AND G H) (AND I J))) False)]

;; Test 05: AND with guard literals (should not transform)
!(assertEqual
    (andCut (OR A B (AND A D C (OR (AND B C) (AND E F))) C (NOT D)) (AND A D C (OR (AND B C) (AND E F))))
    ((OR A B (AND A D C (OR (AND B C) (AND E F))) C (NOT D)) (AND A D C (OR (AND B C) (AND E F))) False))
;; expected Result [((OR A B (AND A D C (OR (AND B C) (AND E F))) C (NOT D)) (AND A D C (OR (AND B C) (AND E F))) False)]