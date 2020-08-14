(define-module (gnu packages nomad)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "84e894eb7945c3bcdf7f8d5135c1be3efa524c92")
        (revision "822")
        (hash     (base32 "1pkcij65zy2lkip5yrfzj85nq17pp9mrf0d4sk6hpjqr4kd0bxd5")))
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
                (sha256 hash))))))

(define-public nomad-git
  (let ((commit "0.2.0-alpha-44-ge58805d")
        (hash   (base32 "18qiqhp9wfxxvlwa8sjcfravdrqpnzylxcc5b6qgampims7v44ky")))
    (package
      (inherit nomad)
      (name "nomad-git")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/nomad.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash)))
      (inputs (cons `("g-golf" ,g-golf-git)
                    (alist-delete "g-golf" (package-inputs nomad)))))))
