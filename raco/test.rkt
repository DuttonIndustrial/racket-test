#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/cmdline
         racket/port
         (planet okcomps/racket-test))


(define (main)
  (define operation 'summary)
  
  (define test-files
    (command-line
     #:program "raco test"
     #:once-any
     [("-l") "List tests in execution set"
                      (set! operation 'list)]
     #:args filenames
     filenames))
  
  (for-each (λ (test-file)
              (load-test-file test-file))
            (if (empty? test-files)
                (find-test-files)
                test-files))
  
  (cond [(equal? operation 'list)
         (for-each (λ (test)
                     (printf "~a~n" test))
                   (tests unit-test?))]
        [(equal? operation 'summary)
         (let loop ([tests (tests unit-test?)]
                    [fail-count 0])
           (if (empty? tests)
               (if (equal? fail-count 0)
                   (exit 0)
                   (exit 1))
               (let* ([test (first tests)]
                      [result (harness-test test)])
                 (if (equal? 'ok (result-summary result))
                     (loop (rest tests) fail-count)
                     (begin
                       (printf "~a in ~a~n~n" (result-summary result) test)
                       (printf "Log:~n")
                       (for-each (λ (log)
                                   (printf "~a~n" log))
                                 (result-log result))
                       (printf "~n~n")
                       (loop (rest tests)
                             (add1 fail-count)))))))]
        [else
         (error "Don't know what to do")]))

(main)
         
         
      

