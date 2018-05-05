(define-module (nomad util)
  #:export (catch-eval home-dir info))

(define (info msg)
  (format #t "INFO: ~a\n" msg))

(define (home-dir)
  (getenv "HOME"))

;; (if (not (getlogin))
;;     (getenv "HOME"))
;;     (passwd:dir (getpwnam (getlogin))))

(define (catch-eval expr)
  (catch #t
    (lambda ()
      (eval-string expr))
    (lambda (key . args)
      (simple-format #f "~s: ~s" key args))))
