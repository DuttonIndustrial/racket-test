#lang racket/base

(require racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/string
         racket/port
         (planet okcomps/racket-test))


(define (filter-tests filters)
  (tests (λ (t)
           (ormap (λ (ft)
                    (ft t))
                  filters))))


(define (main)
  (define operation 'summary)
  (define list-filters empty)
  
  (define test-files
    (command-line
     #:program "raco racket-test"
     #:multi
     [("-t" "--type") type 
                     ("Test type to filter."
                      "Defaults to unit tests."
                      (format "One of: ~a" (string-join (map (compose symbol->string test-type-name) (test-types)) ", ")))
                      (if (equal? (string->symbol type) 'all)
                          (set! list-filters (cons (λ (t) #t) list-filters))
                          (set! list-filters (cons (test-type-filter (get-test-type (string->symbol type))) list-filters)))]
    
     #:once-any
     [("-l" "--list") "List tests in execution set"
           (set! operation 'list)]
     [("-o" "--output") "Harness tests in execution set"
           (set! operation 'harness)]
     #:args filenames
     filenames))
  
  ;default to unit tests if not test types were specified
  (when (empty? list-filters)
    (set! list-filters (list unit-test?)))
  
  (for-each (λ (test-file)
              (load-test-file test-file))
            (if (empty? test-files)
                (find-test-files)
                test-files))
  
  (cond [(equal? operation 'list)
         (for-each (λ (test)
                     (printf "~a~n" test))
                   (filter-tests list-filters))]
        
        [(equal? operation 'summary)
         (let loop ([tests (filter-tests list-filters)]
                    [fail-count 0])
           (if (empty? tests)
                 (if (equal? fail-count 0)
                     (begin 
                       (printf "PASS!~n")
                       (exit 0))
                     (begin
                       (printf "FAIL!~n")
                       (exit 1)))
               (let* ([test (first tests)]
                      [result (test->result test)])
                 (if (equal? 'ok (result-summary result))
                     (loop (rest tests) fail-count)
                     (begin
                       (printf "~a in ~a~n~n" (result-summary result) test)
                       (copy-port result-log (current-output-port))
                       (close-output-port result-log)
                       (printf "~a in ~a~n~n" (result-summary result) test)
                       (printf "~n~n")
                       (loop (rest tests)
                             (add1 fail-count)))))))]
        
        [(equal? operation 'harness)
         (for-each (λ (t)
                     (harness (test-main t)))
                   (filter-tests list-filters))]
        [else
         (error "Don't know what to do")]))

(main)
         
          
         
      

