(define-module (gnu packages nomad)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile-xyz))

(define-public emacsy-git
  (let ((commit "v0.4.1-37-g5f91ee6")
        (hash   (base32 "03ym14g9qhjqmryr5z065kynqm8yhmvnbs2djl6vp3i9cmqln8cl")))
    (package
      (inherit emacsy-minimal)
      (name "emacsy-git")
      (version (string-drop commit 1))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacsy.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash))))))

(define-public g-golf-git
  (let ((commit   "50ec76cd28ef2f97bb5e891dec17cb5a153ad630")
        (revision "879")
        (hash     (base32 "0ynh7803cw9hmirbh63zg6ibd88siq6zq2b6ry15cdhm33r51mia")))
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
  (let ((commit "0.2.0-alpha-195-g340b51f")
        (hash   (base32 "1lgfghhgvr4c8915fp1yg1if7dg4k76zxwmlkh8569w5yvl6kq6a")))
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
      (native-inputs (cons `("gettext" ,gnu-gettext)
                           (package-native-inputs nomad)))
      (inputs (cons* `("g-golf" ,g-golf-git)
                     `("emacsy" ,emacsy-git)
                     `("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
                     (alist-delete "emacsy" (alist-delete "g-golf" (package-inputs nomad))))))))

(define-public nomad-aarch64
  (let ((parent nomad-git))
    (package
      (inherit parent)
      (name "nomad-aarch64")
      (propagated-inputs (alist-delete "gst-plugins-good"
                                       (package-propagated-inputs parent))))))
