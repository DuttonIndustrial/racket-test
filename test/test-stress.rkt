#lang racket

(require (planet okcomps/racket-test))

(define-stress-test stress-exceed-memory
  (test-gc-interval 10000) 
  (test-limit-memory 200000)
  (test-timeout 100000)
  
  (let loop ([lst empty])
    (build-list 100 (λ (x) x))
    (sleep 0.00001)
    (loop (cons (random 100000) lst))))
  
  

(stress-exceed-memory)
  
  