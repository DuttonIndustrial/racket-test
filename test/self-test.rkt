#lang racket


(require (planet okcomps/racket-test))


(define-unit-test gc-interval-test
  (test-gc-interval 1000)
  (printf "hello~n")
  (sleep 10))


(define-unit-test timeout-test
  (test-timeout 250))

(define-unit-test memory-limit-test
  (test-limit-memory 100000)
  (let loop ([lst empty])
    (loop (cons "lots of memory" lst))))

(define-unit-test custodian-shutdown-test
  (custodian-shutdown-all (current-custodian)))

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




