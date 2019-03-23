(define-module (nomad options)
  #:use-module (ice-9 getopt-long)
  #:export (option-url
            option-listen
            display-options))

(define option-spec
  '((listen  (value #t))
    (version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))

(define (get-option key options default)
  (option-ref (getopt-long options option-spec) key default))

(define (option-listen options)
  (get-option 'listen options "/tmp/nomad-socket"))

(define (option-url options)
  (let ((url (get-option '() options #f)))
    (if (null? url)
        #f
         (car url))))
