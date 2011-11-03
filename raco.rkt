#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/cmdline
         racket/port
         (planet okcomps/racket-test))


#|
racket-test ..\racket-test-suite

racket-test c:\Users\cman\dev\openssl

racket-test show c:\Users\cman\dev\racket-test-suite\openssl\test-openssl.rkt

|#

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
  

(define (run-tests (tests (tests))) 
  (for-each (λ (test)
              (run-test test))
            tests))


(define (load-test-file path)
  (let ([eval-ns (make-base-namespace)])
    (namespace-attach-module (current-namespace)
                             '(planet okcomps/racket-test)
                             eval-ns)
    (eval `(require (file ,path)) eval-ns)))


(define (find-test-files (path (current-directory)))
  (fold-files (λ (path type results)
                (if (and (equal? type 'file) (regexp-match #px"^test-.+[.]rkt" (path->string (file-name-from-path path))))
                    (cons (path->string path) results)
                    results))
              empty
              path))


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
                   (tests))]
        [(equal? operation 'summary)
         (let loop ([tests (tests)]
                    [fail-count 0])
           (if (empty? tests)
               (if (equal? fail-count 0)
                   (exit 0)
                   (exit 1))
               (let* ([test (first tests)]
                      [result (run-test test)])
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
         
         
      

