(define-module (gnu packages g-golf)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "91000e065e3d9dac1be816636dd683dd0c66baef")
        (revision "807"))
     (package
      (inherit g-golf)
      (name "g-golf-git")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/g-golf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1agh1wj3idr5p7grxz0fmsdirja6qxi4l5nsqf3f1lkcni0vq5p5")))))))
