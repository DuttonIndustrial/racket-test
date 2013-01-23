#lang racket/base

(require (for-syntax racket/base)
         racket/async-channel
         racket/function
         racket/list
         racket/match
         racket/port
         racket/sandbox
         rackunit
         "test-api.rkt")

(provide harness
         default-harness-timeout
         absolute-harness-timeout)




;absolute harness timeout is the number of seconds after which the harness will timeout
;no matter what else happens, weather the test requests additional time or not
(define absolute-harness-timeout (make-parameter (* 5 60 1000 )))

;the number of secods that the harness allows to the test by default
(define default-harness-timeout (make-parameter 5000))





;this file contains primitive accessable to a test
;this is how the test interacts with its containing harness
;and ultimatly the outside world 
(define-struct gc-info (major? 
                 pre-amount 
                 pre-admin-amount 
                 code-amount       
                 post-amount 
                 post-admin-amount
                 start-process-time 
                 end-process-time
                 start-time 
                 end-time)
  #:prefab)

(define (harness-log . args)
  (write (list* (current-test-instance-id)
                (current-inexact-milliseconds)
                (if (string? (first args))
                    (list (apply format args))
                    args)))
  (newline))


 ;reads current input port until eof found
 ;any special test commands are written to the current output port as special values
 ;all others are written back to the output stream with no modifications
 ;the parse depends upon the first output from the stream to be a special
 ;code that delineats harness commands
 (define (parse-harness-commands output-channel)
   (define x (read-line))
   ;(log-debug (format "parser: processing ~v" x))
   (match (with-input-from-string x read)
     [(and (list-rest 'test code rest) v)
      ;(log-debug (format "parser: code is ~v" code))
      (async-channel-put output-channel v)
      (let ([exp (regexp-quote (format "(~a" code))])
        (let loop ([next-line (read-line)])
          (cond [(equal? eof next-line)
                 (log-debug "parser: exiting")
                 (void)]
                [(regexp-match? exp next-line)
                 (log-debug (format "parser: matched ~v" next-line))
                 (async-channel-put output-channel (with-input-from-string next-line read))
                 (loop (read-line))]
                [else
                 (log-debug (format "parser: passing ~v" next-line))
                 (async-channel-put output-channel next-line)
                 (loop (read-line))])))]
     [else
      (error 'missing-test-instance-id "'(test test-instance-id-number) was not presented as the first item in the output string")]))

 
;takes a test object
;executes the test within a thread and outputs the execution results on standard output
(define (harness thunk)
  (call-with-custodian-shutdown
   (λ ()
     (let*-values 
         ([(gc-log-listener) (make-log-receiver (current-logger) 'debug)]
          [(input-from-test output-from-test) (make-pipe)] ;captures output from the testing thread
          [(test-output-channel) (make-async-channel)] ;parse all captured output from the testing thread
          [(absolute-timeout-evt) (alarm-evt (+ (current-inexact-milliseconds) (absolute-harness-timeout)))]
          
          ;this custodian is used as part of memory tracking and limiting
          ;of memory usage for a test
          [(memory-limit-custodian) (make-custodian)]
          [(memory-limit-custodian-box) (make-custodian-box memory-limit-custodian #t)]
          
          ;the test custodian is wrapped by limit custodian so that the harness can
          ;determine if the memory limit was reached
          [(test-custodian) (make-custodian memory-limit-custodian)]
          
          ;the harness command parser filters out commands from the tests output 
          ;that it understands, and allows it to cooperate with the test
          [(harness-commands-parser) (parameterize ([current-input-port input-from-test])
                                       (thread (λ () (parse-harness-commands test-output-channel))))]
          
          ;finally we launch the thread as part of test-custodian
          ;any output for the thread is captured and added to our test log
          ;any exceptions are captured and logged as well
          [(test-thread) (parameterize ([current-custodian test-custodian]
                                        [current-output-port output-from-test]
                                        [uncaught-exception-handler (λ (exn)
                                                                      (test-log 'error (format "~a" exn))
                                                                      ((error-escape-handler)))]
                                        [current-check-handler raise]) ;for tests that use rackunit, we want to raise the error and capture it
                           (thread thunk))])
                                       
                         
       ;get the test instance id
       (match (async-channel-get test-output-channel)
         [(list-rest 'test test-instance-id rest)
          (current-test-instance-id test-instance-id)
          (apply harness-log 'start rest)
          
          (let loop ([next-timeout (+ (current-inexact-milliseconds) (default-harness-timeout))]
                     [gc-interval #f]
                     [next-gc #f])
            (sync    
             (handle-evt (thread-dead-evt harness-commands-parser)
                         (λ (ev)
                           (error 'internal-error "the had an internal error. harness-commands-parser died")))
                         
             
             (handle-evt (thread-dead-evt test-thread)
                         (λ (ev)
                           (log-debug "harness: test thread dead")
                           
                           ;if the memory-limit-custodian-box was shutdown, it means that limit memory was reached
                           ;and we report it
                           (when (sync/timeout 0 memory-limit-custodian-box)
                             (harness-log 'memory-limit-reached))
                                                      
                           ;shutdown the test and cleanup any resources it may have left behind
                           (custodian-shutdown-all memory-limit-custodian)))
             
             ;when the timeout is reach we log the timeout and stop the test
             (handle-evt (choice-evt absolute-timeout-evt (if next-timeout (alarm-evt next-timeout) never-evt))
                         (λ (v)
                           (if (equal? v absolute-timeout-evt)
                               (log-debug "harness: absolute timeout reached")
                               (log-debug "harness: timeout reached"))
                           (harness-log 'timeout-reached)
                           (custodian-shutdown-all memory-limit-custodian)))
             
             ;when a gc interval timeout occurs, we force garbage collection
             ;this is because the memory usage statistics are only updated when a major gc occurs
             (handle-evt (if next-gc (alarm-evt next-gc) never-evt)
                         (λ (v)
                           (log-debug "harness: collecting garbage")
                           (collect-garbage)
                           (loop next-timeout gc-interval (+ (current-inexact-milliseconds) gc-interval))))
             
             ;when a gc event occurs, we report on gc statistics
             (handle-evt gc-log-listener
                         (λ (gc-data)
                           (loop next-timeout
                                 gc-interval
                                 ;if a major gc occured, we log it and can reset on our gc-interval so we don't force again
                                 (if (match gc-data
                                       [(vector level str (gc-info major? pre-amount pre-admin-amount code-amount post-amount post-admin-amount start-process-time end-process-time start-time end-time))
                                        (harness-log 'gc (current-memory-use memory-limit-custodian) major? start-time end-time)
                                        major?]
                                       [else
                                        next-gc])
                                     (and gc-interval (+ (current-inexact-milliseconds) gc-interval))
                                     gc-interval))))
             
             
             ;if we received any information from the test itself
             (handle-evt test-output-channel
                         (λ (next-msg)
                           (match next-msg
                             ;when we receive a timeout message from our test thread
                             ;we set the timeout limit to the given value
                             ;this allows a test to "keep-alive" if it wants to
                             [(list-rest `,test-instance-id 'timeout timeout-at rest)
                              (log-debug (format "harness: setting timeout to ~v" timeout-at))
                              (apply harness-log 'timeout timeout-at rest)
                              (loop timeout-at gc-interval next-gc)]
                             
                             [(list-rest `,test-instance-id 'gc-interval gc-interval rest)
                              (log-debug (format "harness: setting gc-interval to ~v" gc-interval))
                              (apply harness-log 'gc-interval gc-interval rest)
                              (loop next-timeout gc-interval (current-inexact-milliseconds))]
                             
                             [(list-rest `,test-instance-id 'limit-memory limit rest)
                              (log-debug (format "harness: limiting memory to ~v" limit))
                              (apply harness-log 'limit-memory limit rest)
                              (custodian-limit-memory memory-limit-custodian limit)
                              (loop next-timeout gc-interval next-gc)]
                             
                             ;any time an abort message is logged we halt the test
                             [(list-rest `,test-instance-id 'abort rest)
                              (log-debug "harness: received abort")
                              (apply harness-log 'abort rest)
                              (custodian-shutdown-all memory-limit-custodian)]
                             
                             ;any unknown messages are passed through
                             [(list-rest `,test-instance-id arg rest)
                              (log-debug "harness: recieved ~v" arg)
                              (apply harness-log (list* arg rest))
                              (loop next-timeout gc-interval next-gc)]
                             
                             [else
                              (harness-log else)
                              (loop next-timeout gc-interval next-gc)])))))]
         
         [else
          (error 'missing-test-instance-id "'(test test-instance-id-number) was not presented as the first output from the test. Received ~v" else)])
       
       (close-output-port output-from-test)
       
       (thread-wait harness-commands-parser)
       
       ;complete reading any available input from the test
       (let loop ([next (async-channel-try-get test-output-channel)])
         (match next
           [(list-rest `,test-instance-id rest)
            (apply harness-log rest)
            (loop (async-channel-try-get test-output-channel))]
           [(? string?)
            (write-string next)
            (newline)
            (loop (async-channel-try-get test-output-channel))]
           [else
            (void)]))
       
       ;signal when the test exited
       (harness-log 'end)))))