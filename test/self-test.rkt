#lang racket/base


(require racket/list
         racket/port
         rackunit
         (planet okcomps/racket-test))

(define-stress-test stress-exceed-memory
  (test-limit-memory 50000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))


(define-unit-test self-test-pass
  (printf "here is a message from self-test-pass~n")
  (test-log "hey look another test!")
  (test-log "and another test!")
  (test-log "goobye!"))
  
(define-performance-test perf-test
  (sleep 0.1))

(define-unit-test self-test-abort
  (test-abort "oops this test aborted!"))

(define-unit-test self-test-error
  (error 'oops))

(define-unit-test self-test-timeout
  (test-timeout 0)
  (sleep 1))

(define-unit-test memory-limit-test
  (test-limit-memory 100000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))

(define-unit-test custodian-shutdown-test
  (custodian-shutdown-all (current-custodian)))

(define-unit-test rackunit-fail
  (check-equal? #t #f))

(define-unit-test kill-current-thread-test
  (kill-thread (current-thread)))



(define (self-test test expected)
  (let ([result (test->result test #:output #f)])
    (unless (equal? (result-summary result) expected)
      (copy-port (result-log result) (current-output-port))
      (error "test failed" test))))

(define (main)
  (self-test rackunit-fail 'error)
  (self-test stress-exceed-memory 'memory-limit-reached)
  (self-test self-test-pass 'ok)
  (self-test perf-test 'ok)
  (self-test self-test-abort 'abort)
  (self-test self-test-error 'error)
  (self-test self-test-timeout 'timeout-reached)
  (self-test custodian-shutdown-test 'no-result)
  (self-test kill-current-thread-test 'no-result)
  (printf "SELF TEST PASS!"))
  
(main)


