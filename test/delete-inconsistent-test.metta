!(register-module! ../Reduct-Rules)
!(import! &self Reduct-Rules:delete-inconsistent)

;; Test 01: Direct literal negation (A vs NOT A)
!(assertEqual
    (deleteInconsistentHandle (OR B (AND (NOT A) (OR B C))) (AND (NOT A) (OR B C)) (A))
    ((OR B) () True))
;; expected Result [((OR B) () True)]
;; Test 02: Direct literal negation reverse (NOT A vs A)
!(assertEqual
    (deleteInconsistentHandle (OR B (AND A (OR B C))) (AND A (OR B C)) ((NOT A)))
    ((OR B) () True))
;; expected Result [((OR B) () True)]

;; Test 03: Consistent literals - no negation
!(assertEqual
    (deleteInconsistentHandle (OR B (AND A (OR B C))) (AND A (OR B C)) (B))
    ((OR B (AND A (OR B C))) (AND A (OR B C)) False))
;; expected Result [((OR B (AND A (OR B C))) (AND A (OR B C)) False)]

;; Test 04: Multiple consistent literals
!(assertEqual
    (deleteInconsistentHandle (OR C (AND A B (OR C D))) (AND A B (OR C D)) (D E))
    ((OR C (AND A B (OR C D))) (AND A B (OR C D)) False))
;;expected Result [((OR C (AND A B (OR C D))) (AND A B (OR C D)) False)]

;; Test 05: Multiple inconsistent pairs
!(assertEqual
    (deleteInconsistentHandle (OR X (AND (NOT Y) Z (OR X W))) (AND (NOT Y) Z (OR X W)) (Y))
    ((OR X) () True))
;;expected Result [((OR X) () True)]