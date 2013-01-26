#lang racket/base


(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         racket/match
         srfi/27
         "testing.rkt"
         "test-api.rkt"
         "registry.rkt")

(provide current-iteration-id
         define-performance-test
         make-iteration-id
         mark-iteration-start
         mark-iteration-end
         perf-test?
         parse-iterations)


(struct perf-test test ()
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                      write
                                      display)
                                  (format "Perf: ~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))


(register-test-type 'perf "A performance test typically measures a run-time or iteration times of a piece of code. Typically considered a success if it meets a specific run time goal." perf-test?)

(define current-iteration-id (make-parameter #f))

(define (make-iteration-id)
  (current-iteration-id (random-integer (expt 2 128)))
  (current-iteration-id))


(define (mark-iteration-start (id (make-iteration-id)))
  (collect-garbage)
  (test-log 'perf-start id (current-inexact-milliseconds) (current-process-milliseconds))
  id)

(define (mark-iteration-end (id (current-iteration-id)))
  (test-log 'perf-end id (current-inexact-milliseconds) (current-process-milliseconds)))


(define-syntax (define-performance-test stx)
  (define-splicing-syntax-class maybe-iterations-option
    (pattern (~seq #:iterations count:exact-positive-integer))
    (pattern (~seq) #:with count #'1))
  
  (syntax-parse stx
    [(_ name its:maybe-iterations-option code ...)
     #`(begin
         (define name (perf-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx) 
                       (λ ()
                         (do ((iterations its.count (sub1 iterations)))
                           ((= iterations 0))
                           ((λ () code ...)))
                         (test-ok))))
         (register-test name))]))


#|(define (parse-perf-data-simple log)
  (parse-iterations log)|#


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
    
    