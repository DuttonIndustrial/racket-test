#lang racket/base



(require "../../harness.rkt"
         "../../performance.rkt")


(define-performance-test
  racket-startup
  
  (define racket-path (find-system-path 'exec-file))
  
  (current-subprocess-custodian-mode 'kill)
  
  
  (time-iterations 
   10
   (λ ()
     
     (define-values (sub in out err) (subprocess #f #f #f racket-path "exit-racket.rkt"))
     
     (sync sub)
     
     (unless (equal? (subprocess-status sub) 0)
       (error 'racket-startup "exited with code ~v" (subprocess-status sub)))
     
     (close-input-port in)
     (close-output-port out)
     (close-input-port err))))



(define-performance-test
  racket-base-startup
  
  (define racket-path (find-system-path 'exec-file))
  
  (current-subprocess-custodian-mode 'kill)
  
  (time-iterations 
   10
   (λ ()
     (define-values (sub in out err) (subprocess #f #f #f racket-path "exit-base.rkt"))
     
     (sync sub)
     
     (unless (equal? (subprocess-status sub) 0)
       (error 'racket-startup "exited with code ~v" (subprocess-status sub)))
     
     (close-input-port in)
     (close-output-port out)
     (close-input-port err))))
     



(define-performance-test build-list1
  (time-iterations 
   10
   (λ ()
     (build-list 1 (λ (x) x)))))

(define-performance-test build-list10 
  (time-iterations 
   10
   (λ ()
     (build-list 10 (λ (x) x)))))

(define-performance-test build-list100
  (time-iterations 
   10
   (λ ()
     (build-list 100 (λ (x) x)))))

(define-performance-test build-list1000
  (time-iterations 
   10
   (λ ()
     (build-list 1000 (λ (x) x)))))

(define-performance-test build-list10000
  (time-iterations 
   10
   (λ ()
     (build-list 10000 (λ (x) x)))))

(define-performance-test build-list100000
  (time-iterations 
   10
   (λ ()
     (build-list 100000 (λ (x) x)))))

(define-performance-test build-list1000000
  (time-iterations 
   10
   (λ ()
     (build-list 1000000 (λ (x) x)))))
