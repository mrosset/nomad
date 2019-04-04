(define-module (tests eval)
  #:use-module (nomad eval)
  #:use-module (nomad minibuffer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "eval")

(define-command (alpha-one bar) "" bar)
(define-command (alpha-two bar) "" bar)

(test-equal "input completions"
  (input-completion "alpha") '("alpha-two" "alpha-one"))

(test-assert "is command" (command? alpha-one))
(test-assert "add to command list" (begin
				     (add-to-command-alist 'version version)
				     (eq? (assoc-ref command-alist 'version) version)))

(test-end)
