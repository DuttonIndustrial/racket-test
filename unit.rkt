#lang racket/base

(require (for-syntax racket/base)
         "harness.rkt")

(provide define-unit-test
         unit-test?)



(struct unit-test test ())

(define-syntax (define-unit-test stx)
  (syntax-case stx ()
    [(_ name code ...)
     #`(begin
         (define name (unit-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx) 
                       (λ ()
                         code ...)))
         (register-test name))]))
