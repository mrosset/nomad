(define-module (wemacs util)
  #:export (catch-eval))

(define (catch-eval expr)
  (catch #t
    (lambda ()
      (eval-string expr))
    (lambda (key . args)
      (simple-format #f "~s: ~s" key args))))
