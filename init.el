(require 'package)

(setq user-emacs-directory "~/.wemacs/")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;; Keybindings
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

(global-company-mode)

(setq company-idle-delay nil
      company-minimum-prefix-length 2)

(which-key-mode 1)

(setq initial-major-mode 'scheme-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq
 geiser-guile-binary "guile2.2"
 geiser-active-implementations '(guile)
 geiser-default-implimentation 'guile)

(find-file "~/session.scm")
(geiser-connect-local 'guile "/tmp/wemacs")
(other-window -1)

(eldoc-mode)
(save-place-mode 1)
(persistent-scratch-setup-default)

(setq initial-scratch-message "\
;; This buffer is for text that is not saved, and for Guile Scheme evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n")

;; My custom don't distribute
(require 'evil)
(evil-mode 1)

(setq org-startup-indented t)

(add-hook 'org-mode-hook 'auto-fill-mode)

(org-babel-do-load-languages
'org-babel-load-languages
'((scheme . t)
 (emacs-lisp . t)
 (C . t)
 (sh . t)))
