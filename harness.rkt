#lang racket/base

(require (for-syntax racket/base)
         racket/async-channel
         racket/list
         racket/match
         srfi/27
         (planet okcomps/mailbox))



(provide harness
         current-harness-log-channel
         default-harness-timeout
         test-log-raw ;log a raw message to the current harness
         test-log     ;log a message to the currently running harness
         test-abort
         test-limit-memory
         test-timeout
         test-gc-interval
         current-test-instance-id
         make-test-instance-id
         current-harness-thread
         (struct-out test)
         (struct-out result)
         result-summary
         harness-test
         output-tests)


;this file contains primitive accessable to a test
;this is how the test interacts with its containing harness
;and ultimatly the outside world 





#| make sure that all new test instance id's are completely new
if we did not do this. random-integer would return the same sequence
every time we run the code|#
(random-source-randomize! default-random-source)

#| the intention here is to makes a sufficiently unique number 
such that all test-instance-ids will be unique for all time 
on all computers|#
(define (make-test-instance-id)
  (random-integer (expt 2 128)))

(struct test (name source line thunk)
  #:property prop:procedure (λ (test)
                              (if (current-harness-thread)
                                  ((test-thunk test))
                                  (harness-test test)))
                                
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                     write
                                     display)
                                  (format "~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))


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

(struct result (log)
  #:property prop:custom-write (λ (result port mode)
                                 ((if mode
                                      write
                                      display)
                                  (result-summary result)
                                  port)))

(define current-test-instance-id (make-parameter 0))

(define current-harness-thread (make-parameter #f))


(define (current-test-time)
  (inexact->exact (round (current-inexact-milliseconds))))



(define (test-log-raw args)
  (let ([harness-thread (current-harness-thread)])
    (when harness-thread
      (thread-send harness-thread
                   args
                   (λ ()
                     (error 'log "no harness thread active"))))))


;logs a message to the test harness
(define (test-log . args)
  (test-log-raw (list* (current-test-time) 
                  (current-test-instance-id)
                  (if (string? (first args))
                      (list (apply format args))
                      args))))

;instructs the test harness that an error occured
(define (test-abort . args)
  (apply test-log 'abort args))


;tells the harness to limit memory to size bytes
(define (test-limit-memory size)
  (test-log 'limit-memory size))



;tells the harness to timeout when timeout-at >= (current-test-time)
(define (test-timeout (timeout #f) #:at (timeout-at #f))
  (test-log 'timeout (if timeout
                         (+ (current-test-time) timeout)
                         timeout-at)))



;tells the harness to collect garbage at gc-interval milliseconds
(define (test-gc-interval gc-interval)
  (test-log 'gc-interval gc-interval))

(define default-harness-timeout (make-parameter 30000))

(define current-harness-log-channel (make-parameter #f))


(define (harness-output-message msg)
  (async-channel-put (current-harness-log-channel) msg))
  

;runs a single test within a new harness
;and returns the test result
(define (harness-test test)
  (parameterize ([current-harness-log-channel (make-async-channel)])
    (harness test)
    (result (let loop ([log empty]
                       [next (async-channel-try-get (current-harness-log-channel))])
              (if next
                  (loop (cons next log)
                        (async-channel-try-get (current-harness-log-channel)))
                  (reverse log))))))


(define (output-tests tests)
  (parameterize ([current-harness-log-channel (make-async-channel)])
    (let ([run-thread (thread (λ ()
                                (for-each harness tests)))])
      (let loop ([next (sync run-thread (current-harness-log-channel))])
        (if (equal? next run-thread)
            (let loop ([next (async-channel-try-get (current-harness-log-channel))])
              (when next
                (write next)
                (newline)
                (loop (async-channel-try-get (current-harness-log-channel)))))
            (begin
              (write next)
              (newline)
              (loop (sync run-thread (current-harness-log-channel)))))))))

(define (harness test)
  (parameterize ([current-test-instance-id (make-test-instance-id)]
                 [current-harness-thread (current-thread)])
    
    (test-log 'test (test-name test) (test-source test) (test-line test))
    (let*-values ([(test-output test-output-port) (make-pipe)] ;captures output from the testing thread
                  [(output-buffer) (make-bytes 1024)]
                  
                  ;this custodian is used as part of memory tracking and limiting
                  ;of memory usage for a test
                  [(memory-limit-custodian) (make-custodian)]
                  [(memory-limit-custodian-box) (make-custodian-box memory-limit-custodian #t)]
                  
                  ;the test custodian is wrapped by limit custodian so that the harness can
                  ;determine if the memory limit was reached
                  [(test-custodian) (make-custodian memory-limit-custodian)] 
                  
                  ;finally we launch the thread as part of test-custodian
                  ;any output for the thread is captured and added to our test log
                  ;any exceptions are captured and logged as well
                  [(test-thread) (parameterize ([current-custodian test-custodian]
                                                [current-output-port test-output-port]
                                                [uncaught-exception-handler (λ (ex)
                                                                              (test-abort 'error ex)
                                                                              ((error-escape-handler)))])
                                   (thread test))])
      
      (call-with-exception-handler 
       (λ (exn)
         (custodian-shutdown-all memory-limit-custodian)
         exn)
       (λ ()
         (test-timeout (default-harness-timeout))
         
         (let loop ([next-timeout #f]
                    [gc-interval #f]
                    [next-gc #f])
           (receive 
            
            
            ;when the thread terminates we exit the loop
            [(event (thread-dead-evt test-thread))
             
             ;if the memory-limit-custodian-box was shutdown, it means that limit memory was reached
             ;and we report it
             (when (sync/timeout 0 memory-limit-custodian-box)
               (test-log 'memory-limit-reached))
             
             ;shutdown the test and cleanup any resources it may have left behind
             (custodian-shutdown-all memory-limit-custodian)]
            
            ;when any test thread writes output to its current-output-port
            ;we capture it and inject it into the harness log
            [(event test-output)
             (let ([num-bytes (read-bytes-avail! output-buffer test-output)])
               (test-log 'output (subbytes output-buffer 0 num-bytes))
               (loop next-timeout gc-interval next-gc))]
            
            
            ;when the timeout is reach we log the timeout and stop the test
            [(timeout (and next-timeout (/ (- next-timeout (current-test-time)) 1000)))
             (test-log 'timeout-reached)
             (custodian-shutdown-all test-custodian)
             (loop #f gc-interval next-gc)]
            
            ;when a gc interval timeout occurs, we collect garbage and report 
            ;current memory usage
            [(timeout (and next-gc (/ (- next-gc (current-test-time)) 1000)))
             (collect-garbage)
             (test-log 'gc (current-memory-use memory-limit-custodian))
             (loop next-timeout gc-interval (+ (current-test-time) gc-interval))]
            
            
            ;when we receive a timeout message from our test thread
            ;we set the timeout limit to the given value
            ;this allows a test to "keep-alive" if it wants to
            [(list-rest time id 'timeout timeout-at rest)
             (harness-output-message (list* time id 'timeout timeout-at rest))
             (loop timeout-at gc-interval next-gc)]
            
            [(list-rest time id 'gc-interval gc-interval rest)
             (harness-output-message (list* time id 'gc-interval gc-interval rest))
             (loop next-timeout gc-interval (current-test-time))]
            
            [(list-rest time id 'limit-memory limit rest)
             (harness-output-message (list* time id 'limit-memory limit rest))
             (custodian-limit-memory memory-limit-custodian limit)
             (loop next-timeout gc-interval next-gc)]
            
            ;any time an error message is logged we halt the test
            [(list-rest time id 'abort rest)
             (harness-output-message (list* time id 'abort rest))
             (custodian-shutdown-all test-custodian)
             (loop next-timeout gc-interval next-gc)]
            
            ;any unknown messages are passed through
            [(list-rest time id rest)
             (harness-output-message (list* time id rest))
             (loop next-timeout gc-interval next-gc)]
            
            [else
             (error 'bad-log "unknown message received ~v" else)]))
         
         ;signal when the test exited
         (test-log 'done)
         
         (close-output-port test-output-port)
         
         ;complete reading any available input from the test
         (let loop ()
           (let ([num-bytes (read-bytes-avail! output-buffer test-output)])
             (unless (equal? eof num-bytes)
               (test-log 'output (subbytes output-buffer 0 num-bytes))
               (loop))))
         
         (close-input-port test-output)
         
         ;finish writing any remaining logs out
         (let loop ()
           (when (sync/timeout 0 (thread-receive-evt))
             (harness-output-message (receive))
             (loop))))))))




#|
racket-test ..\racket-test-suite

racket-test c:\Users\cman\dev\openssl

racket-test show c:\Users\cman\dev\racket-test-suite\openssl\test-openssl.rkt

|#
#|
;runs a harness in a subprocess
;and executes the given test
(define (sub-harness-test test)

  (define-values (sub stdout stdin stderr) (subprocess #f #f #f (find-system-path 'exec-file) "-t" "harness.rkt" "-m" (string->path (test-source test)) (symbol->string (test-name test))))
  
  (define out-pump (thread (λ ()
                             (copy-port stdout (current-output-port)))))
  
  (define err-pump (thread (λ ()
                             (copy-port stderr (current-output-port)))))

  (sync sub)
  (sync (thread-dead-evt out-pump))
  
  (unless (equal? (subprocess-status sub) 0)
    (error 'racket-startup "exited with code ~v" (subprocess-status sub)))
     
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr))
|#
