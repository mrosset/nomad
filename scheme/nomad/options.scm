(define-module (nomad options)
  #:use-module (nomad webview)
  #:use-module (ice-9 getopt-long)
  #:export (
            option-app-id
            option-client
            option-listen
            option-url
            ))

(define option-spec
  '((listen  (value #t))
    (client  (single-char #\c) (value #f))
    (app-id  (value #t))
    (version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))

(define (get-option key options default)
  (option-ref (getopt-long options option-spec) key default))

(define (option-app-id options)
  (get-option 'app-id options "org.gnu.nomad"))

(define (option-client options)
  (get-option 'client options #f))

(define (option-listen options)
  (get-option 'listen options "/tmp/nomad-socket"))

(define (option-url options)
  (let ((url (get-option '() options default-home-page)))
    (if (null? url)
        default-home-page
        (car url))))
