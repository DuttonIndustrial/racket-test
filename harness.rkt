#lang racket/base

(require racket/list
         racket/match
         racket/file
         racket/path
         racket/port
         srfi/27
         (for-syntax racket/base)
         (planet okcomps/mailbox))

(provide harness-log            ;log a message to the currently running harness
         harness-error
         harness-limit-memory
         harness-timeout
         harness-gc-interval
         test-harness
         harness-run-tests
         tests
         (struct-out test)
         register-test
         current-test-instance
         main
         define-test
         current-log-thread
         default-harness-timeout)


(random-source-randomize! default-random-source)

(struct test (name source line thunk)
  #:property prop:procedure (struct-field-index thunk)
  #:property prop:custom-write (λ (test port mode)
                                 ((if mode
                                     write
                                     display)
                                  (format "~a @ ~a line ~a" (test-name test) (test-source test) (test-line test)) port)))


(define registered-tests empty)

(define (tests (test-filter (λ (t)
                         #t)))
  (reverse (filter test-filter registered-tests)))

(define (register-test test)
  (set! registered-tests (cons test registered-tests)))

(define-syntax (define-test stx)
  (syntax-case stx ()
    [(_ name code ...)
     #`(begin
         (define name (test 
                       'name 
                       #,(path->string (syntax-source stx) )
                       #,(syntax-line stx) 
                       (λ ()
                         (let ([test (λ () code ...)])
                           (if (current-log-thread)
                               (test)
                               (test-harness name))))))
         (register-test name))]))




(define current-test-instance (make-parameter 0))

(define current-log-thread (make-parameter #f))

(define default-harness-timeout (make-parameter 30000))


(define (writeln msg)
  (write msg)
  (newline))

(define (log-raw args)
  (let ([log-thread (current-log-thread)])
    (when log-thread
      (thread-send log-thread
                   args
                   (λ ()
                     (error 'log "no logging thread active"))))))


;logs a message to the test harness
(define (harness-log . args)
  (log-raw (list* (current-inexact-milliseconds) 
                  (current-test-instance)
                  (if (string? (first args))
                      (list (apply format args))
                      args))))

;instructs the test harness that an error occured
(define (harness-error . args)
  (apply harness-log 'error args))


;tells the harness to limit memory to size bytes
(define (harness-limit-memory size)
  (harness-log 'limit-memory size))



;tells the harness to timeout when timeout-at >= (current-inexact-milliseconds)
(define (harness-timeout (timeout #f) #:at (timeout-at #f))
  (harness-log 'timeout (if timeout
                         (+ (current-inexact-milliseconds) timeout)
                         timeout-at)))



;tells the harness to collect garbage at gc-interval milliseconds
(define (harness-gc-interval gc-interval)
  (harness-log 'gc-interval gc-interval))
  


(define (test-harness test)
  (parameterize ([current-test-instance (random-integer (expt 2 128))]
                 [current-log-thread (current-thread)])
    
    (harness-log 'test (test-name test) (test-source test) (test-line test))
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
                                                                              (harness-error (exn-message ex))
                                                                              ((error-escape-handler)))])
                                   (thread test))])
      
      (harness-timeout (default-harness-timeout))
        
      (let loop ([next-timeout #f]
                 [gc-interval #f]
                 [next-gc #f])
        (receive 
         
         
         ;when the thread terminates we exit the loop
         [(event (thread-dead-evt test-thread))
          
          ;if the memory-limit-custodian-box was shutdown, it means that limit memory was reached
          ;and we report it
          (when (sync/timeout 0 memory-limit-custodian-box)
            (harness-log 'memory-limit-reached))
          
          ;shutdown the test and cleanup any resources it may have left behind
          (custodian-shutdown-all memory-limit-custodian)]
         
         ;when any test thread writes output to its current-output-port
         ;we capture it and inject it into the harness log
         [(event test-output)
          (let ([num-bytes (read-bytes-avail! output-buffer test-output)])
            (harness-log 'output (subbytes output-buffer 0 num-bytes))
            (loop next-timeout gc-interval next-gc))]
         
         
         ;when the timeout is reach we log the timeout and stop the test
         [(timeout (and next-timeout (/ (- next-timeout (current-inexact-milliseconds)) 1000)))
          (harness-log 'timeout-reached)
          (custodian-shutdown-all test-custodian)
          (loop #f gc-interval next-gc)]
         
         ;when a gc interval timeout occurs, we collect garbage and report 
         ;current memory usage
         [(timeout (and next-gc (/ (- next-gc (current-inexact-milliseconds)) 1000)))
          (collect-garbage)
          (harness-log 'gc (current-memory-use memory-limit-custodian))
          (loop next-timeout gc-interval (+ (current-inexact-milliseconds) gc-interval))]
         
         
         ;when we receive a timeout message from our test thread
         ;we set the timeout limit to the given value
         ;this allows a test to "keep-alive" if it wants to
         [(list-rest time id 'timeout timeout-at rest)
          (writeln (list* time id 'timeout timeout-at rest))
          (loop timeout-at gc-interval next-gc)]
         
         [(list-rest time id 'gc-interval gc-interval rest)
          (writeln (list* time id 'gc-interval gc-interval rest))
          (loop next-timeout gc-interval (current-inexact-milliseconds))]
         
         [(list-rest time id 'limit-memory limit rest)
          (writeln (list* time id 'limit-memory limit rest))
          (custodian-limit-memory memory-limit-custodian limit)
          (loop next-timeout gc-interval next-gc)]
         
         ;any time an error message is logged we halt the test
         [(list-rest time id 'error rest)
          (writeln (list* time id 'error rest))
          (custodian-shutdown-all test-custodian)
          (loop next-timeout gc-interval next-gc)]
         
         ;any unknown messages are passed through
         [(list-rest time id rest)
          (writeln (list* time id rest))
          (loop next-timeout gc-interval next-gc)]
         
         [else
          (error 'bad-log "unknown message received ~v" else)]))
      
      ;signal when the test exited
      (harness-log 'done)
      
      (close-output-port test-output-port)
      
      ;complete reading any available input from the test
      (let loop ()
        (let ([num-bytes (read-bytes-avail! output-buffer test-output)])
          (unless (equal? eof num-bytes)
            (harness-log 'output (subbytes output-buffer 0 num-bytes))
            (loop))))
      
      (close-input-port test-output)
      
      ;finish writing any remaining logs out
      (let loop ()
        (when (sync/timeout 0 (thread-receive-evt))
          (writeln (receive))
          (loop))))))


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
  

(define (harness-run-tests (tests (tests))) 
  (for-each (λ (test)
              (test-harness test))
            tests))

(define (find-test-files (path (current-directory)))
  (fold-files (λ (path type results)
                (if (and (equal? type 'file) (regexp-match #px"^test-.+[.]rkt" (path->string (file-name-from-path path))))
                    (cons path results)
                    results))
              empty
              path))


(define (load-test-file path)
  (let ([eval-ns (make-base-namespace)])
    (namespace-attach-module (current-namespace)
                             "harness.rkt"
                             eval-ns)
    (eval `(require (file ,path)) eval-ns)))
              



(define (main module-name (test-to-run #f))
  (printf "Loading ~v~n" module-name)
  (load-test-file (string->path module-name))
  (printf "~v~n" (tests))
  (harness-run-tests (tests (λ (test)
                      (if test-to-run
                          (equal? (symbol->string (test-name test)) test-to-run)
                          #t)))))


#|
(map load-test-file (find-test-files))
(sub-harness-test (first (tests)))
|#

#|
(load-test-file "c:\\Users\\cman\\dev\\racket-test-suite\\perf\\test-baseline.rkt")
(tests)
|#
