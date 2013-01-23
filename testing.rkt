#lang racket/base

(require racket/match
         racket/port
         "test-api.rkt"
         "harness.rkt")

(provide parse-test-log
         test->result
         (struct-out test)
         result-id
         result-start-time
         result-end-time
         result-summary
         result-reason
         (rename-out (result-log-port result-log))
         test-main)

#|
  this module provides the basic functionality for all new types of tests
|#
(define (test->result test #:output (output #f))
  (let-values ([(input-from-harness harness-output) (make-pipe)]
               [(result-i result-o) (make-pipe)])
    
    (parameterize ([current-output-port harness-output])
      (thread (λ () 
                (harness (test-main test))
                (close-output-port (current-output-port)))))
    
    (thread (λ () 
              (if output
                  (copy-port input-from-harness result-o (current-output-port))
                  (copy-port input-from-harness result-o))
              (close-output-port result-o)))
                         
    (parameterize ([current-input-port result-i])
      (parse-test-log))))
         

(define (test-main test)
  (λ ()
    (current-test-instance-id (make-test-instance-id))
    (current-test-start-time (current-inexact-milliseconds))
    (write (list 'test (current-test-instance-id) test))
    (newline)
    ((test-thunk test))))
  
  
  


(struct test (name source line thunk)
  #:property prop:procedure (λ (test)
                              (test->result test #:output #t))
                                
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                     write
                                     display)
                                  (format "~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))


(struct result (test id start-time end-time summary reason log-bytes)
  #:property prop:custom-write (λ (result port mode)
                                 (define output (if mode write display))
                                 (output (format "Result ~x for test ~a: ~a" (result-id result) (result-test result) (result-summary result)) port)
                                 (newline port)
                                 (unless (equal? (result-summary result) 'ok)
                                   (output (result-reason result) port)
                                   (newline port))))
                                 

(define (result-log-port result)
  (open-input-bytes (result-log-bytes result)))




(define (parse-test-log (input (current-input-port)))
  (let-values ([(log-pipe-in log-pipe-out) (make-pipe)])
             (match (with-input-from-string (read-line) read)
               [(and (list-rest test-id start-time 'start test-info) start-msg)
                (log-debug (format "result parser: test-id is ~v - test-info is ~v" test-id test-info))
                (write start-msg log-pipe-out)
                (newline log-pipe-out)
                (let loop ([next (read-line)]
                           [end-time #f]
                           [summary 'no-result]
                           [result-info ""])
                  (log-debug (format "result parser: processing ~v" next))
                  (match (if (eof-object? next)
                             (error 'unexpected-oef "reached eof before test log end marker")
                             (with-input-from-string next read))
                    
                    [(and (list-rest `,test-id end-time 'end rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (close-output-port log-pipe-out)
                     (let ([log-bytes (port->bytes log-pipe-in)])
                       (close-input-port log-pipe-in)
                       (result test-info test-id start-time end-time summary result-info log-bytes))]
                    
                    [(and (list-rest `,test-id time 'abort rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time (if (equal? summary 'no-result) 'abort summary) rest)]
                    
                    [(and (list-rest `,test-id time 'error rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time (if (equal? summary 'no-result) 'error summary) rest )]
                    
                    [(and (list-rest `,test-id time 'timeout-reached rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time (if (equal? summary 'no-result) 'timeout-reached summary) rest)]
                    
                    [(and (list-rest `,test-id time 'memory-limit-reached rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time (if (equal? summary 'no-result) 'memory-limit-reached summary) rest)]
                    
                    [(and (list-rest `,test-id time 'ok rest) msg)
                     (write msg log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time (if (equal? summary 'no-result) 'ok summary) rest)]
                    
                    [else
                     (log-debug (format "result parser: skipped ~v" next))
                     (write else log-pipe-out)
                     (newline log-pipe-out)
                     (loop (read-line) end-time summary result-info)]))]
               [else
                (error 'expected-test-start "expected a test start message. started with ~v" else)])))

         
                 




