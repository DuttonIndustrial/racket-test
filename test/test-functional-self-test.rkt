#lang racket/base


(require (planet okcomps/racket-test))


(define-unit-test self-test-fail
  (test-abort "oops this test fails!"))

(define-unit-test self-test-error
  (raise 'oops))


(define-unit-test self-test-pass
  (printf "YAY ITS ALL GOOD"))

(define-unit-test self-test-timeout
  (test-timeout 0)
  (sleep 1))
  