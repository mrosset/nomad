(define wemacs-test
  (lambda()
    (format #t "starting Wemacs: version ~a" (wemacs-version))
    (wemacs-start)))
