(define-module (gnu packages nomad)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile-xyz))

(define-public emacsy-git
  (let ((commit "v0.4.1-33-ge90ede5")
        (hash   (base32 "01kb2glfl8y12f551jk9sjc6sbj73yxzd8xrlshylsp2pgyqm9ic")))
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
  (let ((commit "0.2.0-alpha-139-ge36c04b")
        (hash   (base32 "1i502wmlfckx889kyidgv71ig8kbd4b562nni06yi0dcr065lb6n")))
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
