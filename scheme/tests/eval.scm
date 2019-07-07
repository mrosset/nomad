(define-module (tests eval)
  #:use-module (nomad eval)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64))

(test-begin "eval")

(test-equal "eval-input pass"
  "#t"
  (receive (value err)
      (input-eval "#t")
    value))

(test-equal "eval-input fail"
  #nil
  (receive (value err)
      (input-eval "adsfadfadadfadfsadf")
    value))

(test-end)
