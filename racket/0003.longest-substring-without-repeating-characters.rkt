#lang racket

(require data/queue)

; @lc code=begin

(define/contract (length-of-longest-substring s)
  (-> string? exact-integer?)

  (let ([queue (make-queue)] ;; queue to keep track of valid substring
        [table (mutable-set)]) ;; table to keep track of elements in the substring

    (for/fold ([res 0]) ([c (in-string s)]) ;; loop through characters of s

      (for ([_ (in-naturals)]
            ;; loop while c is in table
            #:break (not (set-member? table c)))

        (let ([top (dequeue! queue)])
          ;; remove the element from table
          (set-remove! table top)))

      ;; add the new element in the queue and table
      (enqueue! queue c)
      (set-add! table c)

      (max res (queue-length queue))))
  )

; @lc code=end
