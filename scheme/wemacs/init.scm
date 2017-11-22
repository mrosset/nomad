(define-module (wemacs init)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 pretty-print)
  #:use-module (wemacs keymap)
  #:use-module (wemacs events)
  #:export (init run-tests user-init-file))

(define user-init-file (string-append (getenv "HOME") "/.wemacs.scm"))

(define (run-tests)
  (pretty-print (all-threads))
  (current-thread))

(define (init)
  (add-hook! key-press-hook handle-key-press)
  (add-hook! key-press-hook debug-key-press)
  (add-hook! event-hook debug-event)
  (if (file-exists? user-init-file)
      (load user-init-file)))

 ;; (repl-start)
 ;; (browser-start))
 ;;  (let ((t (begin-thread (repl-start) (browser-start))))
 ;;    (while #t (sleep 1))))
 ;; ;; (let ((t (make-thread (browser-start))))
 ;;   (repl-start)
 ;;    (join-thread t)))
