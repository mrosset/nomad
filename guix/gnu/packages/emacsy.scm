(define-module (gnu packages emacsy)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile-xyz))

(define-public emacsy-git
  (let ((commit "d459ca1d3d09e7624e662bc4cfc3596850796fc6"))
    (package
      (inherit emacsy)
      (name "emacsy-minimal")
      (version (git-version "v0.4.1" "28" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacsy.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ps15w8cxj9kc18gmvys9jv9xa1qqa7m43ismv34l3cmhddrn0sr")))))))
