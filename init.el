(setq wemacs-hosted t)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package geiser
  :config
  (setq
   geiser-guile-binary "guile2.2"
   geiser-active-implementations '(guile)
   geiser-default-implimentation 'guile))

(geiser-connect-local 'guile "/tmp/wemacs")
(delete-window)
(find-file "~/session.scm")
