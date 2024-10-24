#lang racket

; @lc code=begin

(define (vector-ref-or-inf vec idx len)
  (cond
    [(< idx 0) -inf.0]
    [(>= idx len) +inf.0]
    [else (vector-ref vec idx)]))

(define/contract (find-median-sorted-arrays nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) flonum?)

  (let* {[a1 (list->vector nums1)] ;; convert the lists to vector for fast indexing
         [a2 (list->vector nums2)]
         [n1 (length nums1)]
         [n2 (length nums2)]
         [left (quotient (+ n1 n2 1) 2)] ;; number of elements in the left group (after partitioning)

         [l 0] [r n1] ;; left and right pointers for binary search
         [sol-found #f]}

    (for {[_ (in-naturals)]
          #:break (or (> l r) sol-found)}

      (let* {[mid1 (quotient (+ l r) 2)] ;; take mid1 amount from a1
             [mid2 (- left mid1)] ;; rest from a2
             [l1 (vector-ref-or-inf a1 (sub1 mid1) n1)]
             [l2 (vector-ref-or-inf a2 (sub1 mid2) n2)]
             [r1 (vector-ref-or-inf a1 mid1 n1)]
             [r2 (vector-ref-or-inf a2 mid2 n2)]}

        (cond
          ;; check if this partition is correct
          ;; (all the left elements should be less all the right elements)
          [(and (<= l1 r2) (<= l2 r1))
           (set! sol-found
                 (if (even? (+ n1 n2))
                     (/ (+ (max l1 l2) (min r1 r2)) 2.0)
                     (max l1 l2)))]

          [(> l1 r2) (set! r (sub1 mid1))]

          [else (set! l (add1 mid1))])
        ))

    (exact->inexact sol-found))
  )

; @lc code=end

(find-median-sorted-arrays '(1 2) '(3 4))