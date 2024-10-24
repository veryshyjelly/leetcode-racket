#lang racket


; Definition for singly-linked list:

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

; @lc code=begin

;; get-val returns value or zero if the list is null
(define (get-val li)
  (if (list-node? li) (list-node-val li) 0))

;; get-next returns #f or the next node
(define (get-next li)
  (and li (list-node-next li)))

;; add-two-helper adds the two lists and return the result in ordinary list
(define (add-two-helper l1 l2 carry acc)
  (define sum (+ (get-val l1) (get-val l2) carry))

  (cond
    [(and (= sum 0) (nor l1 l2)) acc]
    [else (add-two-helper (get-next l1)
                          (get-next l2)
                          (quotient sum 10)
                          (cons (remainder sum 10) acc))]))

;; make-list makes a new linked-list from the given list
(define (make-list l acc)
  (match l
    ['() acc]
    [(cons h t) (define new-node (list-node h acc))
                (make-list t new-node)]))

(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))

  ;; first sum the two lists and store it in our own list
  (define sum (add-two-helper l1 l2 0 '()))

  ;; then form a linked list from the resulting list
  (make-list sum #f))

; @lc code=end
