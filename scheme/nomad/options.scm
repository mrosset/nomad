(define-module (nomad options)
  #:use-module (nomad webview)
  #:use-module (ice-9 getopt-long)
  #:export (
            option-app-id
            option-client
            option-platform
            option-listen
            option-url
            option-quick
            ))

(define option-spec
  '((listen  (value #t))
    (gapplication-app-id (value #t))
    (help-all (value #f))
    (platform (value #t))
    (client  (single-char #\c) (value #f))
    (quick   (single-char #\Q) (value #f))
    (version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))

(define (get-option key args default)
  (option-ref (getopt-long args option-spec) key default))

(define (option-platform args)
  (get-option 'platform args "gtk"))

(define (option-quick args)
  (get-option 'quick args #f))

(define (option-app-id args)
  (get-option 'gapplication-app-id args "org.gnu.nomad"))

(define (option-client options)
  (get-option 'client options #f))

(define (option-listen args)
  (get-option 'listen args "/tmp/nomad-socket"))

(define (option-url args)
  (let ((url (get-option '() args default-home-page)))
    (if (null? url)
        default-home-page
        (car url))))
