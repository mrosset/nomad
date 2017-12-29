(define-module (nomad util)
  #:export (catch-eval))

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

(define (catch-eval expr)
  (catch #t
    (lambda ()
      (eval-string expr))
    (lambda (key . args)
      (simple-format #f "~s: ~s" key args))))
