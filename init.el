;; init.el
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
(menu-bar-mode -1)
(setq nomad-hosted t)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(use-package geiser
  :ensure t
  :config
  (setq
   ;; FIXME: don't assume guile binary is 'guile'
   geiser-guile-binary "guile"
   geiser-active-implementations '(guile)
   geiser-default-implimentation 'guile))

(use-package which-key
  :delight
  :config (which-key-mode 1))

(use-package emacs
  :init
  (setq inhibit-startup-message t)
  :config

  ;; FIXME: don't assume socket file location
  (geiser-connect-local 'guile "/tmp/nomad-socket")
  (delete-other-windows))
