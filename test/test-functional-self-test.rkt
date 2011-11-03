#lang racket/base


(require (planet okcomps/racket-test))


(define-test self-test-fail
  (test-error "oops this test fails!"))