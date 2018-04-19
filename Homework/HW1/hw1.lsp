;; function takes an argument n, the index of element in the sequence
;; if n <= 2, then it is a base case and function returns 1
;; otherwise, function returns the sum of PAD(n-3) and PAD(n-2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameter:                                 ;;
;;      n   -- integer, index of the element  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns:                                   ;;
;;      value of the nth element              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PAD (n)
    (cond 
       ;; base cases: PAD(0) = PAD(1) = PAD(2) = 1
       ((= n 0) 1)
       ((= n 1) 1)
       ((= n 2) 1)
       ;; PAD(n+1) = PAD(n-1) + PAD(n-2)
       (t (+ (PAD (- n 3)) (PAD (- n 2))))
    )
)

;; function takes an argument n, the index of element in the sequence
;; if n <= 2, it is a base case and function returns 0 (no addition needed)
;; otherwise, compute the sum of times of additions needed to get PAD(n-3)
;; and PAD(n-2), and then increment the sum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameter:                                 ;;
;;      n   -- integer, index of element      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns:                                   ;;
;;      number of additions needed            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SUMS (n)
    (cond
        ;; base cases: 0 addition needed
        ((= n 0) 0)
        ((= n 1) 0)
        ((= n 2) 0)
        ;; compute SUMS(n-3) + SUMS(n-2) and then increment the value
        (t (+ (+ (SUMS (- n 3)) (SUMS  (- n 2))) 1))
    )
)

;; function takes an argument 'tree', which is a list representing a tree structure
;; keep caring the list until it becomes empty. 
;; If the tree argument is an atom, then return '?. If the tree argument is an
;; empty list, returns nil 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameter:                                 ;;
;;      tree    -- list, tree structure       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns:                                   ;;
;;      anonymized tree with same struction   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ANON (tree)
    (cond
        ;; if empty list, return nil
        ((null tree) nil)
        ;; if atom, return ?
        ((atom tree) '?)
        ;; the result is:
        ;; cons ANON(the first atom of tree) and ANON(the rest of tree)
        (t (cons (ANON (car tree)) (ANON (cdr tree))))
    )
)