(define-module (gnu packages nomad)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages guile-xyz))

(define-public g-golf-git
  (let ((commit   "4e9847582367e66ba2aae07507765855e0f9c3d6")
        (revision "816")
        (hash     (base32 "05pppka6bqswpymydf5wpphm5v0ijlv36apg6mqcwbs2vjf8dnqy")))
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
  (let ((commit "0.1.2-alpha-337-g8962d4d")
        (hash   (base32 "0x7vpa8vnjgccm92p7l6yw25i1659ywig9pyar995ggr378dx1al")))
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
