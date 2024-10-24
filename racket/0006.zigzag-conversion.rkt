#lang racket

(require data/gvector)

; @lc code=begin

(define (zig-zag s numRows)
  (define rows (make-vector numRows #f))

  (for ([i (in-range numRows)])
    (vector-set! rows i (make-gvector)))

  (for ([c (in-string s)]
        [i (in-naturals)])
    (define row (abs
                 (- (sub1 numRows)
                    (remainder i (* (sub1 numRows) 2)))))

    (gvector-add! (vector-ref rows row) c))

  (string-join (reverse
                (for/list ([row (in-vector rows)])
                  (list->string (gvector->list row)))) "")
  )

(define/contract (convert s numRows)
  (-> string? exact-integer? string?)
  (if (= numRows 1)
      s
      (zig-zag s numRows))
  )

; @lc code=end

(convert "PAYPALISHIRING" 3)
