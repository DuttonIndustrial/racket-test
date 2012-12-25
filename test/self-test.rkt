#lang racket/base


(require racket/list
         racket/port
         (planet okcomps/racket-test))

(define-stress-test stress-exceed-memory
  (test-limit-memory 50000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))


(define-unit-test self-test-pass
  (printf "here is a message from selt-test-pass~n")
  (test-log "hey look another test!")
  (test-log "and another test!")
  (test-log "goobye!"))
  
(define-performance-test perf-test
  (sleep 0.1)
  (test-ok))

(define-unit-test self-test-fail
  (test-abort "oops this test fails!"))

(define-unit-test self-test-error
  (raise 'oops))

(define-unit-test self-test-timeout
  (test-timeout 0)
  (sleep 1))

(define-unit-test memory-limit-test
  (test-limit-memory 100000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))

(define-unit-test custodian-shutdown-test
  (custodian-shutdown-all (current-custodian)))


#|



(define-unit-test kill-current-thread-test
  (kill-thread (current-thread)))


(define-unit-test abort-test
  (test-abort "abort occurred"))

(define-unit-test exception-test
  (error 'oops "uh oh an exception happend"))

(define-unit-test absolute-timeout-test
  (test-timeout 1000)
  (sleep 100000))

(define-unit-test ok-test
  (test-log "yay its all ok ~a" 'ummm-kkkaaaayyy))
|#

(define (self-test test expected)
  (let-values ([(result output) (test->result-summary test)])
    (unless (equal? (result-summary-result result) expected)
      (copy-port output (current-output-port))
      (error "~a test failed" test))))

(define (main)
  (self-test stress-exceed-memory 'memory-limit-reached)
  (self-test self-test-pass 'ok)
  (self-test perf-test 'ok)
  (self-test self-test-fail 'abort)
  (self-test self-test-error 'error)
  (self-test self-test-timeout 'timeout-reached)
  (self-test custodian-shutdown-test 'no-result))
  
(main)
;(error "Self Test Failed!")

