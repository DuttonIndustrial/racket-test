#lang racket/base


(require (for-syntax racket/base
                     syntax/parse)
         "test-api.rkt"
         "test.rkt"
         "registry.rkt")


(provide define-stress-test
         stress-test?
         stress-desired-run-time)


(define (stress-desired-run-time time)
  (test-log 'desired-runtime time))
  
(struct stress-test test ())

(register-test-type 'stress "A stress test usually attempts to measure performance under load. Typically the test is considered pass if it runs longer than a specified duration of time." stress-test?)
 


(define-syntax (define-stress-test stx)
  (syntax-parse stx
    [(_ name code ...)
     #`(begin
         (define name (stress-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx)
                       (λ ()
                         code ...)))
         (register-test name))]))