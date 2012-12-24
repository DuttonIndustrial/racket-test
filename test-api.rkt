#lang racket/base

(require racket/list
         racket/match)


(provide test-ok      ;logs a test completed ok message
         test-log     ;log a message to the currently running harness
         test-abort   ;aborts a test and halts test execution
         test-limit-memory ;sets a memory limit on the test
         test-timeout ;causes a test to timeout in a given amount of time
         test-gc-interval 
         current-test-time
         current-test-start-time
         current-test-instance-id)


(define current-test-instance-id (make-parameter 0))


;the number of seconds that have expired since the test began
(define current-test-start-time (make-parameter #f))


(define (current-test-time)
  (- (current-inexact-milliseconds) (current-test-start-time)))

;logs a message to the test harness
(define (test-log . args)
  (newline)
  (write (if (string? (first args))
             (list (current-test-instance-id) (apply format args))
             (list* (current-test-instance-id) args)))
  (newline)
  (flush-output))

(define (test-ok . args)
  (apply test-log 'ok args))


;instructs the test harness that an error occured
(define (test-abort . args)
  (apply test-log 'abort args))


;tells the harness to limit memory to size bytes
(define (test-limit-memory size)
  (test-log 'limit-memory size))

;tells the harness to timeout when timeout-at >= (current-test-time)
(define (test-timeout (timeout #f) #:at (timeout-at #f))
  (test-log 'timeout (if timeout
                         (+ (current-inexact-milliseconds) timeout)
                         timeout-at)))


;tells the harness to collect garbage at gc-interval milliseconds
(define (test-gc-interval gc-interval)
  (test-log 'gc-interval gc-interval))

