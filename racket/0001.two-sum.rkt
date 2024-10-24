#lang racket

; @lc code=begin

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))

  (for/fold ([seen (hash)] ;; hash table to keep track of the indexes of seen elements
             [res #f]
             #:result res)
            ([n (in-list nums)]
             [i (in-naturals)]
             #:break res)

    (define m (- target n))

    (if (hash-has-key? seen m)
        (values seen (list (hash-ref seen m) i))
        (values (hash-set seen n i) #f))
    ))

; @lc code=end
