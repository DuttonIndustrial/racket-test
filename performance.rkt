#lang racket/base


(require (for-syntax racket/base)
         racket/list
         racket/match
         srfi/27
         "harness.rkt")

(provide define-performance-test
         make-iteration-id
         mark-iteration-start
         mark-iteration-end
         (struct-out perf-test)
         time-iteration
         time-iterations
         parse-iterations)


(struct perf-test test ()
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                      write
                                      display)
                                  (format "Perf: ~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))

(define (make-iteration-id)
  (random-integer (expt 2 128)))


(define (mark-iteration-start id)
  (harness-log 'perf-start id))

(define (mark-iteration-end id)
  (harness-log 'perf-end id))


(define (time-iteration thunk)
  (let ([iteration-id (make-iteration-id)])
    (mark-iteration-start iteration-id)
    (thunk)
    (mark-iteration-end iteration-id)))


(define (time-iterations count thunk)
  (let loop ([count count])
    (when (> count 0)
      (time-iteration thunk)
      (loop (sub1 count)))))



(define-syntax (define-performance-test stx)
  (syntax-case stx ()
    [(_ name code ...)
     #`(begin
         (define name (perf-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx) 
                       (λ ()
                         (let ([test (λ () code ...)])
                           (if (current-log-thread)
                               (test)
                               (test-harness name))))))
         (register-test name))]))



(define (parse-iterations log)
  (let loop ([iterations (make-immutable-hash empty)]
             [log log])
    (if (empty? log)
        (hash->list iterations)
        (match (first log)
          [(list time test-id 'perf-start iteration-id)
           (loop (hash-set iterations iteration-id time) (rest log))]
          [(list time test-id 'perf-end iteration-id)
           (loop (hash-set iterations iteration-id (list (hash-ref iterations iteration-id) time (-  time (hash-ref iterations iteration-id)))) (rest log))]
          [else
           (loop iterations (rest log))]))))
    
    