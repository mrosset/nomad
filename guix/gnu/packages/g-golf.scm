(define-module (gnu packages g-golf)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "654991ca911caf56e692df40e2e1cc44c22011ea")
        (revision "751"))
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
                  "00xqgl1a7ca0g1pk6jjavmricj5xwpgwgq2ghpn1711hbcm48088"))))
      (arguments
       (substitute-keyword-arguments (package-arguments g-golf)
         ;; Tests are disabled since they require a DISPLAY
         ((#:tests? _ #f) #f))))))
