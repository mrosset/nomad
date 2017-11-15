;; Browser Keys
(defun wemacs-send(code)
  (interactive)
  (with-temp-buffer
    (geiser-mode)
    (insert code)
    (geiser-eval-buffer)))

(setq browse-url-browser-function 'wemacs-browse-function)
;; (setq browse-url-browser-function 'browse-url-xdg-open)

(defun wemacs-browse ()
  (interactive)
  (let ((url (read-string "url? ")))
  (wemacs-send (format "(browse \"%s\")" url))))

(defun wemacs-browse-function(url &optional args)
  (wemacs-send (format "(browse \"%s\")" url)))

(defun wemacs-back()
  (interactive)
  (wemacs-send "(back)"))

(defun wemacs-forward()
  (interactive)
  (wemacs-send "(forward)"))

(defun wemacs-query()
  (interactive)
  (let ((q (read-string "wemacs? ")))
    (wemacs-send (format "(query \"%s\")" q))))

(defun wemacs-connect ()
  (interactive)
    (geiser-connect-local 'guile "/tmp/wemacs"))

(defun wemacs-reload.el ()
  "Reloads wemacs.el. wemacs.el contains functions for
interacting with geiser and wemacs.  TODO: Promote this function
to emacs minor-mode and melpa package"
  (interactive)
  (load "~/src/wemacs/wemacs.el"))

(defun wemacs-download ()
  "Download url to ~/Downloads.
This is just a place holder and uses emacs url-copy-file. At one
point we may want this to be done by scheme? or webkit?."
  (interactive)
  (let* ((url (read-string "download? "))(file (file-name-nondirectory url)))
    (url-copy-file url (format "~/Downloads/%s" file))))

(global-set-key (kbd "C-c h") 'wemacs-back)
(global-set-key (kbd "C-c l") 'wemacs-forward)
(global-set-key (kbd "C-c s") 'wemacs-query)
