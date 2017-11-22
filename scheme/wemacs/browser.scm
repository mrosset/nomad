(define-module (wemacs browser)
  #:export (
            scroll-up
            scroll-down
            browser-start
            search-provider-format
            browse
            forward
            home
            reload
            back
            query
            wemacs-kill))

(define search-provider-format "https://google.ca/search?q=~a")

(define (browse url)
  (let ((prefix "https://"))
    (if (not (string-prefix? prefix url))
        (set! url (string-append prefix url)))
    (web-view-load-uri url)))

(define (forward)
  (web-view-go-forward))

(define (home)
  (web-view-load-uri "https://www.gnu.org/software/emacs"))

(define (reload)
  (web-view-reload))

(define (back)
  "go back in history"
  (web-view-go-back))

(define (query arg)
  (let ((uri (simple-format #f search-provider-format arg)))
    (browse uri)))
