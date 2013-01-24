#lang racket/base

(require racket/list
         racket/file
         racket/path
         "testing.rkt"
         "test-api.rkt")

(provide register-test
         register-test-type
         test-types
         tests
         load-test-file
         test-type-name
         test-type-description
         test-type-filter
         find-test-files
         get-test-type)


(define registered-test-types empty)

(struct test-type (name description filter) #:transparent)

(define (register-test-type name description filter)
  (set! registered-test-types (cons (test-type name description filter) registered-test-types)))

(define (test-types)
  registered-test-types)

(register-test-type 'all "All Tests" (λ (x) #t))


#| contains the list of registered tests
this is built up at compile time
for each module that is required and
that has one or more define-test like definitions in it|#
(define registered-tests empty)


;returns the set of tests in the order they were registered
(define (tests (test-filter (λ (t) #t)))
  (reverse (filter test-filter registered-tests)))


;registered a test within the testing system
(define (register-test test)
  (unless (test? test)
    (raise-type-error 'register-test "test" test))
  (set! registered-tests (cons test registered-tests)))



(define (load-test-file path)
  (let ([eval-ns (make-base-namespace)])
    (namespace-attach-module (current-namespace)
                             '(planet okcomps/racket-test)
                             eval-ns)
    (eval `(require (file ,path)) eval-ns)))


(define (find-test-files (path (current-directory)))
  (fold-files (λ (path type results)
                (if (and (equal? type 'file) (regexp-match #px"^test.*[.]rkt" (path->string (file-name-from-path path))))
                    (begin
                      (log-debug (format "found test file ~v" (path->string path)))
                      (cons (path->string path) results))
                    results))
              empty
              path))


(define (get-test-type type-name)
  (or (findf (λ (type)
               (equal? (test-type-name type) type-name))
             (test-types))  
      (error 'unknown-test-type "Given test type name ~v unknown." type-name)))


