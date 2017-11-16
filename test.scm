(use-modules (ice-9 rdelim))
(use-modules (ice-9 threads))
(use-modules (ice-9 match))
(use-modules (system repl server))

(define client-socket (socket PF_UNIX SOCK_STREAM 0))
(define socket-path "/tmp/wemacs")
(define search-provider-format "https://google.com/search?q=~a")

(define start-repl
  (lambda()
    (if (file-exists? "/tmp/guile-socket")
        (delete-file "/tmp/guile-socket"))
    (spawn-server
     (make-unix-domain-server-socket))))

(define (browse url)
  (let ((prefix "https://"))
    (if (not (string-prefix? prefix url))
        (set! url (string-append prefix url)))
    (web-view-load-uri url)))

(define (forward)
  (web-view-go-forward))

(define (home)
  (web-view-load-uri "https://www.gnu.org/software/emacs"))

(define (reload)
  (web-view-reload))

(define (back)
  (web-view-go-back))

(define (query arg)
  (let ((uri (simple-format #f search-provider-format arg)))
    (browse uri)))

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

(define (make-wemacs)
  (make-thread wemacs-start))

(define (start-server)
  (make-thread (run-server)))
