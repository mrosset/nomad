(use-modules (ice-9 rdelim))
(use-modules (ice-9 threads))
(use-modules (ice-9 match))
(use-modules (ice-9 eval-string))
(use-modules (system repl server))
(add-to-load-path "/home/mrosset/src/wemacs/scheme")
(use-modules (wemacs keymap))

(define client-socket (socket PF_UNIX SOCK_STREAM 0))
(define socket-path "/tmp/wemacs")
(define search-provider-format "https://google.ca/search?q=~a")


(define (quasi-eval string)
  "Evaluate non expression string. The string is split into a list by
whitespaces and then passed to eval as a Scheme expression. The car of
the list is the symbol of the procedure to eval and the tail of the list
are joined as one arguement string."
  (let* ((lst (string-split string #\space))
         (proc (string->symbol (car lst)))
         (args  (list-tail lst 1)))
    (if (>= (length args) 1)
        (catch-eval (list proc (string-join args)))
        (catch-eval (list proc)))))

(define (reload-init)
  (load "test.scm"))

(define (ok-if-exception string)
  (false-if-exception (eval-string string)))

(define start-repl
  (lambda ()
  (if (file-exists? "/tmp/guile-socket")
      (delete-file "/tmp/guile-socket"))
  (spawn-server
   (make-unix-domain-server-socket))))

(define (message msg)
  (connect client-socket AF_UNIX socket-path)
  (simple-format client-socket "~a\n" msg)
  (close-port client-socket))

(define server-socket #f)

(define (eval-line client-port)
  (let ((line (read-line client-port)))
    (false-if-exception (eval-string line))))

(define (run-server)
  (when (file-exists? socket-path)
    (delete-file socket-path))

  (set! server-socket (make-unix-domain-server-socket #:path socket-path))

  (sigaction SIGPIPE SIG_IGN)
  (listen server-socket 1)
  (let loop ()
    (match (accept server-socket)
      (#f
       (close server-socket))
      ((client-port . client-addr)
       (eval-line client-port)
       (close-port client-port)
       (loop)))))

(define (wemacs-restart)
  (wemacs-kill)
  (wemacs-start))

(define (make-wemacs)
  (make-thread wemacs-start))

(define (start-server)
  (make-thread (run-server)))
