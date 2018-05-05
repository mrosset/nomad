(define-module (nomad repl)
  #:use-module (ice-9 threads)
  #:use-module (system repl server)
  #:use-module (system repl coop-server)
  #:export (server-start server-force-delete))

(define (server-start)
  (if (file-exists? "/tmp/guile-socket")
      (delete-file "/tmp/guile-socket"))
  (spawn-server
   (make-unix-domain-server-socket)))

(define (server-force-delete)
  (stop-server-and-clients!))
