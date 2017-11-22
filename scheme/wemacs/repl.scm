(define-module (wemacs repl)
  #:use-module (wemacs events)
  #:use-module (ice-9 threads)
  #:use-module (system repl server)
  #:use-module (system repl coop-server)
  #:export (repl-start))

;; (define server (spawn-coop-repl-server (make-unix-domain-server-socket)))

(define (repl-start)
  (if (file-exists? "/tmp/guile-socket")
      (delete-file "/tmp/guile-socket"))
  (spawn-server
   (make-unix-domain-server-socket)))

(define (poll-server-start)
  (make-thread (poll-server-run)))

(define (poll-server-run)
  (while #t
    (run-hook event-hook "(poll-server)")
    (poll-coop-repl-server server)
    (sleep 1)))
