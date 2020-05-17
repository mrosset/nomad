(define-module (gnu packages g-golf)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "30148389eed34a06f16e679be9ae59d9782d559f")
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
                  "1yzfwv5fdpzszv0crx2n8biavqmhmn3nyivzqqv8zkx27wq8b1qb"))))
      (arguments
       (substitute-keyword-arguments (package-arguments g-golf)
         ;; Tests are disabled since they require a DISPLAY
         ((#:tests? _ #f) #f))))))
