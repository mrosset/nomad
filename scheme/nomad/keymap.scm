(define-module (nomad keymap)
  #:use-module (nomad browser)
  #:export (key-press-hook handle-key-press debug-key-press))

(define modifier-masks '((4 . "C") ;; Control
                         ))
(define emacs-keymap '(
                       ("C-u" . (back))
                       ("C-m" . (forward))
                       ("C-n" . (scroll-down))
                       ("C-p" . (scroll-up))
                       ("C-r" . (reload))
                       ))

(define default-keymap emacs-keymap)

(define key-press-hook (make-hook 2))

(define (handle-pair input pair)
  (let* ((key (car pair)) (proc (cdr pair)))
    (when (string= input key)
      (eval proc (interaction-environment)))))

(define (modifier-key->string mod key)
  "Returns a string formatted as key-map key. This looks up the
modifiers bit and returns its string representation, and then formats
it together with the key string. So given the arguments 4 c it would
return \"C-c\". When the modifer is not found in the modifer-masks it returns #f"
  (let ((mod-string (assoc-ref modifier-masks mod)))
    (if (not (eq? mod-string #f))
        (simple-format #f "~a-~a" mod-string key)
        #f)))

(define (handle-key-press mod key)
  (let* ((mod-key (modifier-key->string mod key))
        (proc (assoc-ref default-keymap mod-key)))
        (if (eq? proc #f)
        (simple-format #f "~s : key not found" key)
        (eval proc (interaction-environment)))))

(define (debug-key-press mod key)
  (simple-format #t "thread: ~s mod: ~s key: ~s\n" (current-thread)  mod key))
