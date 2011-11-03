#lang racket/base


(require (for-syntax racket/base)
         "harness.rkt")


(provide define-stress-test
         (struct-out stress-test))


(define (stress-desired-run-time time)
  (test-log 'desired-runtime time))
  


(struct stress-test test ()
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                     write
                                     display)
                                  (format "Stress: ~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))



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
