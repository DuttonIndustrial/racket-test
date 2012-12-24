#lang racket/base

(require (for-syntax racket/base)
         "registry.rkt"
         "test-api.rkt"
         "harness.rkt")

(provide define-unit-test
         unit-test?)

(struct unit-test test ())


(register-test-type 'unit "A functional, unit, or regression test. Typically pass/fail to indicate proper functionality" unit-test?)


(define-syntax (define-unit-test stx)
  (syntax-case stx ()
    [(_ name code ...)
     #`(begin
         (define name (unit-test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx) 
                       (λ ()
                         code ...
                         (test-ok))))
         (register-test name))]))
