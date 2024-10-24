#lang racket

; @lc code=begin

(define/contract (longest-palindrome s)
  (-> string? string?)
  (define n (string-length s))
  (define dp  (make-vector (* (add1 n) n) #f))

  ;; a function mapping a 2d index to a 1d index
  (define (xy->index x y)
    (+ x (* n y)))

  (for ([i (in-range n)])
    (vector-set! dp (xy->index 1 i) #t))

  (for ([i (in-range (sub1 n))])
    (vector-set! dp (xy->index 2 i)
                 (char=? (string-ref s i) (string-ref s (add1 i)))))


  (for* ([l (in-range 3 (add1 n))]
         [i (in-range n)])

    (define is-palin?
      (and (< (+ i l (- 1)) n)
           (char=? (string-ref s i) (string-ref s (+ i l (- 1))))
           (vector-ref dp (xy->index (- l 2) (add1 i)))))

    (vector-set! dp (xy->index l i) is-palin?))

  (define res-list
    (for*/list ([l (in-range 1 (add1 n))]
                [i (in-range n)])
      (and (vector-ref dp (xy->index l i)) (cons i l))))

  (define res (last (filter identity res-list)))
  (substring s (car res) (+ (car res) (cdr res))))

; @lc code=end

(longest-palindrome "a")
(longest-palindrome "babad")
(longest-palindrome "cbbd")
(longest-palindrome "aaaa")
