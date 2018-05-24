;; Browser Keys
(defun nomad-send(code)
  (interactive)
  (with-temp-buffer
    (geiser-mode)
    (insert code)
    (geiser-eval-buffer)))

(setq nomad-process nil)

(defun nomad-connect ()
  (when (nomad-process nil)))

;; (setq nomad-process (make-network-process
;;                       :name "nomad-process"
;;                       :family 'local
;;                       :buffer (get-buffer-create "*nomad-process*")
;;                       :remote "/tmp/guile-socket"))

;; (setq browse-url-browser-function 'nomad-browse-function)
(setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-url-xdg-open)

(defun nomad-browse ()
  (interactive)
  (let ((url (read-string "url? ")))
  (nomad-send (format "(browse \"%s\")" url))))

(defun nomad-browse-function(url &optional args)
  (nomad-send (format "(browse \"%s\")" url)))

(defun nomad-back()
  (interactive)
  (nomad-send "(back)"))

(defun nomad-forward()
  (interactive)
  (nomad-send "(forward)"))

(defun nomad-query()
  (interactive)
  (let ((q (read-string "nomad? ")))
    (nomad-send (format "(query \"%s\")" q))))

(defun nomad-connect ()
  (interactive)
    (geiser-connect-local 'guile "/tmp/nomad-socket"))

(defun nomad-reload.el ()
  "Reloads nomad.el. nomad.el contains functions for
interacting with geiser and nomad.  TODO: Promote this function
to emacs minor-mode and melpa package"
  (interactive)
  (load "~/src/nomad/nomad.el"))

(defun nomad-download ()
  "Download url to ~/Downloads.
This is just a place holder and uses emacs url-copy-file. At one
point we may want this to be done by scheme? or webkit?."
  (interactive)
  (let* ((url (read-string "download? "))(file (file-name-nondirectory url)))
    (url-copy-file url (format "~/Downloads/%s" file))))

(global-set-key (kbd "C-c h") 'nomad-back)
(global-set-key (kbd "C-c l") 'nomad-forward)
(global-set-key (kbd "C-c s") 'nomad-query)
(global-set-key (kbd "C-c o") 'nomad-browse)
