(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package emacs
  :config

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (setq initial-scratch-message "\
;; This buffer is for text that is not saved, and for Guile Scheme evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n")


  (eldoc-mode)
  (save-place-mode 1)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)
  (defvaralias 'c-basic-offset 'tab-width)

  ;; Keybindings
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-c l") 'comment-line)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
  (setq initial-major-mode 'scheme-mode))

(use-package company
  :ensure t
  :config (global-company-mode))

;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (auto-complete-mode t)

;; add lisp autocomplete-support
;; (require 'ac-geiser)
;; (add-hook 'scheme-mode-hook 'ac-geiser-setup)

;; (add-hook 'after-init-hook (lambda()
;;                              (global-company-mode)
;;                              (setq company-idle-delay nil
;;                                    company-minimum-prefix-length 2)
;;                              (add-to-list 'company-backends 'company-elisp)))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package persitant-scratch
  :config
  (persistent-scratch-setup-default))

(use-package geiser 
  :config
  (setq
   geiser-guile-binary "guile2.2"
   geiser-active-implementations '(guile)
   geiser-default-implimentation 'guile))

(geiser-connect-local 'guile "/tmp/wemacs")
(find-file-noselect "init.el")
(find-file-noselect "wemacs.el")
(find-file-noselect "~/session.scm")
(delete-other-windows)
(display-about-screen)

;; My custom don't distribute
(require 'evil)
(evil-mode 1)

(ido-mode 1)
(setq ido-seperator "\n")

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package helm 
  :ensure t
  :config
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode 1))

(setq org-startup-indented t)

(add-hook 'org-mode-hook 'auto-fill-mode)

(org-babel-do-load-languages
'org-babel-load-languages
'((scheme . t)
 (emacs-lisp . t)
 (C . t)
 (sh . t)))

(load-theme 'leuven t)

(load "~/src/wemacs/wemacs.el")
