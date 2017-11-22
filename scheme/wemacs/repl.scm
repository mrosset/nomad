(define-module (wemacs repl)
  #:use-module (system repl server)
  #:export (repl-start))

(define repl-start
  (lambda ()
  (if (file-exists? "/tmp/guile-socket")
      (delete-file "/tmp/guile-socket"))
  (spawn-server
   (make-unix-domain-server-socket))))
