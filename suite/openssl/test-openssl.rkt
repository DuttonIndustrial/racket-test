#lang racket


(require racket/tcp
         rackunit
         "../../harness.rkt"
         "../../stress.rkt"
         "../../performance.rkt")


(define-stress-test 
 serve-serial-connection

  (harness-limit-memory 100000)
  (harness-timeout 30000)
  (harness-gc-interval 5000)
  
  (define server (thread (λ ()
                           (define listener (tcp-listen 16453 10))
                           (let loop ()
                             (let-values ([(in out) (tcp-accept listener)])
                               (let ([msg (read in)])
                                 (write msg out))
                               (close-input-port in)
                               (close-output-port out))
                             (loop)))))
 
 (let loop ([count 0])
   (let-values ([(in out) (tcp-connect "localhost" 16453)])
     (write "hello" out)
     (close-output-port out)
     (read in)
     (close-input-port in)
     (sleep .01)
     (when (= (modulo count 1000) 0)
       (printf "~v connections made" count))
     (loop (add1 count)))))

(define-performance-test
  connect-hangup
  
  (define server (thread (λ ()
                           (define listener (tcp-listen 16453))
                           (let loop ()
                             (let-values ([(in out) (tcp-accept listener)])
                               (read in)
                               (close-input-port in)
                               (write 'ok out)
                               (close-output-port out))
                             (loop)))))
  
  (time-iterations 100
                   (λ ()
                    (let-values ([(in out) (tcp-connect "localhost" 16453)])
                      (write 'ok out)
                      (close-output-port out)
                      (read in)
                      (close-input-port in)))))
                     

;times how long it takes to transfer 100Mb accross the wire
(define-performance-test
  connection-megabyte-throughput
  
  (define server (thread (λ ()
                           (define listener (tcp-listen 16453))
                           (let loop ()
                             (let-values ([(in out) (tcp-accept listener)])
                               (let ([bstr (make-bytes 8192)]) 
                                 (let transfer-loop ([count 0])
                                   (let ([bytes-read (read-bytes-avail! bstr in)])
                                     (if (equal? eof bytes-read)
                                         (printf "read ~v bytes~n" count)
                                         (transfer-loop (+ count bytes-read)))))
                                 (close-input-port in)
                                 (write 'ok out)
                                 (close-output-port out)))
                             (loop)))))
                           
  (time-iterations 10 (λ ()
                    (let-values ([(in out) (tcp-connect "localhost" 16453)])
                      (let ([bstr (make-bytes 8192)])
                        (let transfer-loop ([count 0])
                          (if (< count (expt 2 25))
                              (transfer-loop (+ count (write-bytes bstr out)))
                              (printf "write ~v bytes~n" count)))
                        (close-output-port out)
                        (read in)
                        (close-input-port in))))))