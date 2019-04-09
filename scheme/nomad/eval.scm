;; eval.scm
;; Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

;; This file is part of Nomad

;; Nomad is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Nomad is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nomad eval)
  #:use-module (ice-9 session)
  #:use-module (nomad events)
  #:export (
	    define-alias
	    define-command))

(define-public command-alist '())

(define-public (add-to-command-alist key proc)
  (set-procedure-property! proc 'command #t)
  (set! command-alist (assoc-set! command-alist key proc)))

(define-public command-hook (make-hook 1))

(define-public (make-command key)
  "Adds procedure with the symbol key to command-alist"
  (let ((proc (eval key (interaction-environment))))
    (add-to-command-alist key proc)))

(define-syntax define-command
  (syntax-rules ()
    ((define-command (proc) doc body)
     (begin
       (define-public (proc)
	 doc
	 (run-hook event-hook (format #f "~s" proc))
	 body)
       (add-to-command-alist (procedure-name proc) proc))
     )
    ((define-command (proc arg) doc body)
     (begin
       (define-public (proc arg)
	 doc
	 (run-hook command-hook arg)
	 (run-hook event-hook (format #f "~s" proc))
	 body)
       (add-to-command-alist (procedure-name proc) proc))
     )))

(define-syntax define-alias
  (syntax-rules ()
    ((define-alias alias proc)
     (begin
       (add-to-command-alist (quote alias) proc)
       (define-public alias proc)))))

(define-public (command-ref key)
  "Returns the associated proc by key from command-alist"
  (assoc-ref command-alist key))

(define-public (command-args key)
  "Return a list of required arguments for procedure by `key'"
  (assoc-ref  (procedure-arguments (command-ref key)) 'required))

(define-public (command? proc)
  (if (procedure? proc)
      (procedure-property proc 'command)
      #f))

(define-public (command->string sym)
  (when (command? sym)
      (symbol->string (procedure-name sym))))

(define-public (input-eval input)
  (let* ((result #nil) (error #nil))
    (catch #t
      (lambda ()
	(set! result (format #f "~a" (eval-string input))))
      (lambda (key . parameters)
	    (set! error (format #f "Uncaught throw to '~a: ~a\n" key parameters))))
    (values result error)))
