#lang racket/base

(require racket/list
         racket/match
         "harness.rkt")

(provide (struct-out result)
         result-summary)


;runs a single test within a new harness
;and returns the test result
(define (test->result test #:verbosity (verbose #f))
  (parameterize ([current-harness-log-channel (make-async-channel)])
    (let ([harness-thread (thread (λ () (harness test)))])
      (let loop ([log empty]
                 [next (sync (current-harness-log-channel)
                             harness-thread)])
        (if (equal? next harness-thread)
            (let loop ([log log]
                       [next (async-channel-try-get (current-harness-log-channel))])
              (if next
                  (loop (cons next log)
                        (async-channel-try-get (current-harness-log-channel)))
                  (result test (reverse log))))
            [else
             (when verbose
               (printf "~v~n" next))
             (loop (cons next log)
                   (sync (current-harness-log-channel) harness-thread))])))))
  

(define (result-summary result)
  (let loop ([test-log (result-log result)])
    (if (empty? test-log)
        (error 'bad-log)
        (match (first test-log)
          [(list-rest time id 'memory-limit-reached rest)
           'memory-limit-reached]
          [(list-rest time id 'timeout-reached rest)
           'timeout-reached]
          [(list-rest time id 'abort rest)
           'abort]
          [(list-rest time id 'error rest)
           'error]
          [(list-rest time id 'done rest)
           'ok]
          [else
           (loop (rest test-log))]))))

(define (result-id result)
  (let ([test-log (result-log result)])
    (if (empty? test-log)
        (error 'bad-log)
        (match (first test-log)
          [(list-rest time id rest)
           id]
          [else
           (error 'bad-log)]))))

(struct result (test log)
  #:property prop:custom-write (λ (result port mode)
                                 ((if mode
                                      write
                                      display)
                                  (result-summary result)
                                  port)))
         
         