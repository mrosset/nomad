;; keymap.scm
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

(define-module (nomad keymap)
  #:use-module (ice-9 threads)
  #:use-module (nomad app)
  #:use-module (nomad browser)
  #:use-module (nomad minibuffer)
  #:use-module (nomad window)
  #:export (key-press-hook debug-key-press))

(define modifier-masks '((20 . "C")
			 (24 . "M")
			 (33554456 . "M")
			 (33554452 . "C")))

(define-public ctl-x-map '(("k" . (kill-buffer))
			   ("b" . (next-buffer))
			   ("C-k" . (kill-some-buffers))
			   ("C-b" . (list-buffers))))

(define global-map-old '(("M-x" . (execute-extended-command))
			    ("C-g" . (keyboard-quit))
			    ("C-x" . (which-key-popup ctl-x-map))
			    ("C-h" . (which-key-popup ctl-h-map))))

(define-public ctl-h-map '(("k" . (kill-buffer))
			   ("b" . (next-buffer))))

(define key-masks '((66 . "b")
		    (75 . "k")
		    (77 . "m")
		    (78 . "n")
		    (80 . "p")
		    (82 . "r")
		    (85 . "u")
		    (88 . "x")))

(define key-press-hook (make-hook 3))

(define (modifier-key->string mod key)
  "Returns a string formatted as key-map key. This looks up the
modifiers bit and returns its string representation, and then formats
it together with the key string. So given the arguments 4 c it would
return \"C-c\". When the modifer is not found in the modifer-masks it returns #f"
  (let* ((mod-string (assoc-ref modifier-masks mod))
	 (key-string (assoc-ref key-masks key)))
    (if mod-string
	(simple-format #f "~a-~a" mod-string key)
	key)))

(define-public (handle-key-press keymap mod key)
  (let* ((mod-key (modifier-key->string mod key))
	 (proc (assoc-ref keymap mod-key)))
    (if (eq? proc #f)
	(message (simple-format #f "~s : key not found in ~a"
				mod-key keymap))
	(eval proc (interaction-environment)))))

(define (debug-key-press keymap mod key)
  (message (simple-format #f
			  "mod: ~s key: ~s map: ~s thread: ~s"
			  mod
			  key
			  (modifier-key->string mod key)
			  (current-thread))))
