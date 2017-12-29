(define-module (nomad server))

(define client-socket (socket PF_UNIX SOCK_STREAM 0))
(define socket-path "/tmp/nomad")

(define (message msg)
  (connect client-socket AF_UNIX socket-path)
  (simple-format client-socket "~a\n" msg)
  (close-port client-socket))

(define server-socket #f)

(define (eval-line client-port)
  (let ((line (read-line client-port)))
    (false-if-exception (eval-string line))))

;; (define (run-server)
;;   (when (file-exists? socket-path)
;;     (delete-file socket-path))

;;   (set! server-socket (make-unix-domain-server-socket #:path socket-path))

;;   (sigaction SIGPIPE SIG_IGN)
;;   (listen server-socket 1)
;;   (let loop ()
;;     (match (accept server-socket)
;;       (#f
;;        (close server-socket))
;;       ((client-port . client-addr)
;;        (eval-line client-port)
;;        (close-port client-port)
;;        (loop)))))
