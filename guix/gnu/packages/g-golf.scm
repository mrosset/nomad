(define-module (gnu packages g-golf)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "e99f40e15f6f9e08a6ca5235a5dc002ba4ca999b")
        (revision "0"))
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
                  "0aph503xsb4q82hagnvslvw2017lvgvw9g3ffgz2zy55rq40y8q1")))))))
