(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (gnu packages guile-xyz))

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
                (sha256 hash))))))
