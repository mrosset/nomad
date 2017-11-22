(define-module (wemacs events)
  #:use-module (ice-9 threads)
  #:export (event-hook debug-event))

(define event-hook (make-hook 1))

(define (debug-event event)
  (simple-format #t "thread: ~s event: ~s\n" (current-thread) event))
