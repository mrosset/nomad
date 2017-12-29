(define-module (nomad browser)
  #:use-module (nomad events)
  #:export (
            default-home-page
            current-url
            browser-run
            browser-start
            scroll-up
            scroll-down
            search-provider-format
            browse
            forward
            home
            reload
            back
            query))

(define search-provider-format "https://duckduckgo.com/?q=~a")
(define default-home-page "https://www.gnu.org/software/guile")

(define (browse url)
  (let ((prefix "https://"))
    (if (not (string-prefix? prefix url))
        (set! url (string-append prefix url)))
    (web-view-load-uri url)))

(define (forward)
  (web-view-go-forward))

(define (home)
  (web-view-load-uri default-home-page))

(define (reload)
  (web-view-reload))

(define (back)
  "go back in history"
  (run-hook event-hook "(back)")
  (web-view-go-back))

(define (query arg)
  (let ((uri (simple-format #f search-provider-format arg)))
    (browse uri)))

(define (current-url)
  (web-view-current-url))
