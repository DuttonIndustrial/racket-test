#lang racket/base


(require (for-syntax racket/base)
         "test-api.rkt"
         "registry.rkt"
         "harness.rkt")


(provide define-stress-test
         stress-test?
         stress-desired-run-time)


(define (stress-desired-run-time time)
  (test-log 'desired-runtime time))
  
(struct stress-test test ())

(register-test-type 'stress "A stress test usually attempts to measure performance under load. Typically the test is considered pass if it runs longer than a specified duration of time." stress-test?)
 
#|
#:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                     write
                                     display)
                                  (format "Stress: ~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))|#



(define-syntax (define-stress-test stx)
  (syntax-case stx ()
    [(_ name code ...)
     #`(begin
         (define name (stress-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx)
                       (λ ()
                         code ...)))
         (register-test name))]))