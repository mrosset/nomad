(use-modules (system repl server))
(use-modules (ice-9 readline))
(use-modules (web uri))

(define socket-path "/tmp/wemacs")

(define search-provider-format "https://google.com/search?q=~a")

(define start-repl
  (lambda()
    (if (file-exists? socket-path)
        (delete-file socket-path))

    (spawn-server
     (make-unix-domain-server-socket #:path socket-path))))

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
