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
  #:use-module (nomad browser)
  #:use-module (nomad app)
  #:use-module (ice-9 threads)
  #:export (key-press-hook handle-key-press debug-key-press))

(define modifier-masks '((67108864 . "C")))

(define key-masks '((66 . "b")
                    (77 . "m")
                    (78 . "n")
                    (80 . "p")
                    (85 . "u")
                    (88 . "x")))

(define emacs-keymap '(("C-b" . (next-buffer))
                       ("C-u" . (back))
                       ("C-m" . (forward))
                       ("C-n" . (scroll-down))
                       ("C-f" . (hints))
                       ("C-p" . (scroll-up))
                       ("C-r" . (reload))
                       ("C-x" . (kill-buffer))))

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
  (let* ((mod-string (assoc-ref modifier-masks mod))
         (key-string (assoc-ref key-masks key)))
    (if mod-string
        (simple-format #f "~a-~a" mod-string key-string)
        #f)))

(define (handle-key-press mod key)
  (let* ((mod-key (modifier-key->string mod key))
         (proc (assoc-ref default-keymap mod-key)))
    (if (eq? proc #f)
        (simple-format #f "~s : key not found" key)
        (eval proc (interaction-environment)))))

(define (debug-key-press mod key)
  (simple-format #t "thread: ~s mod: ~s key: ~s\n" (current-thread)  mod key))
