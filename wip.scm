(define (sys . args)
  "Call an external process and raise an error if it returns non-zero."
  (let ((result (apply system* args)))
    (unless (= 0 result)
      (raise-exception (cons 'process-failed args)))))

(define (spring-wip)
  (with-exception-handler
   (lambda (exc) (format #t "Failed. ~a\n" exc))
   (lambda ()
     (sys "stack" "build")
     (sys "stack" "runhaskell" "src/SpringTest.hs")
     (sys "gnuplot" "plot-spring.plt"))
   #:unwind? #t)
  (sys "inotifywait" "-e" "modify"
       "--exclude" "'flycheck_.*'"
       "-r"
       "src" "plot-spring.plt")
  (spring-wip))

(define (ac-wip)
  (with-exception-handler
   (lambda (exc) (format #t "Failed. ~a\n" exc))
   (lambda ()
     (sys "stack" "build")
     (sys "stack" "runhaskell" "src/AcTest.hs")
     (sys "gnuplot" "plot-ac.plt"))
   #:unwind? #t)
  (sys "inotifywait" "-e" "modify"
       "--exclude" ".*flycheck_.*"
       "-r"
       "src" "plot-ac.plt")
  (ac-wip))

;; Still can't figure out how to quit from inotifywait
;; (sigaction SIGINT (lambda (x) (primitive-exit)))

(ac-wip)