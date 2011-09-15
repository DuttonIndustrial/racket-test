#lang racket


(require "harness.rkt"
         "harness-log.rkt")


(define-test gc-interval-test
  (harness-gc-interval 500)
  (sleep 1))


(define-test timeout-test
  (harness-timeout 250)
  (sleep 10))

(define-test memory-limit-test
  (harness-gc-interval 1000)
  (harness-limit-memory 100000000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))

(define-test custodian-shutdown-test
  (custodian-shutdown-all (current-custodian)))

(define-test kill-current-thread-test
  (kill-thread (current-thread)))


(define-test error-test
  (harness-error "oops an error occurred"))

(define-test exception-test
  (error 'oops "uh oh an exception happend"))


(define log (with-input-from-string
             (with-output-to-string
              (λ () (harness-run-tests)))
             port->list))

(test-log-summary log)
