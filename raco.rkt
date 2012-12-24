#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/cmdline
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
                      "Test type to operate on. Defaults to unit tests."
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
  
  (printf "operation :~v~n" operation)
  
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
                     (exit 0)
                     (exit 1))
               (let* ([test (first tests)]
                      [result (test->result-summary test)])
                 (if (equal? 'ok (result-summary-result result))
                     (loop (rest tests) fail-count)
                     (begin
                       (printf "~a in ~a~n~n" (result-summary-result result) test)
                       (printf "~n~n")
                       (loop (rest tests)
                             (add1 fail-count)))))))]
        
        [(equal? operation 'harness)
         (printf "harnessing tests~n")
         (for-each (λ (t)
                     (harness t))
                   (filter-tests list-filters))]
        [else
         (error "Don't know what to do")]))

(main)
         
          
         
      

