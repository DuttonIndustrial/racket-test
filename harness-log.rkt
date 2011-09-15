#lang racket/base



(require racket/list
         racket/match)

(provide parse-test-log-result
         parse-test-log-name
         test-log-summary
         split-test-log)



(define (test-log-summary test-log)
  (map (λ (log)
         (append (parse-test-log-name log)
                (list (parse-test-log-result log))))
       (split-test-log test-log)))



;parses a test output log
;breaks the log into a list of test results
(define (parse-test-log-result test-log)
  (let loop ([test-log test-log])
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



(define (parse-test-log-name test-log)
  (if (empty? test-log)
      (error 'bad-log)
      (match (first test-log)
        [(list-rest time id 'test name source line rest)
         (list name source line)]
        [else
         (error 'bad-log)])))




(define (split-test-log test-log)
  (let loop ([test-log test-log]
             [result-logs (make-immutable-hash empty)])
    (if (empty? test-log)
        (map reverse (hash-values result-logs))
        (match (first test-log)
          [(list-rest time id lst)
           (loop (rest test-log)
                 (hash-set result-logs id (cons (list* time id lst) (hash-ref result-logs id empty))))]
          [else
           'bad-log]))))
    
    

