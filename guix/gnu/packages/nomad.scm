(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages qt)
 (gnu packages pkg-config)
 (gnu packages glib)
 (gnu packages autotools)
 (gnu packages gettext)
 (gnu packages emacs-xyz)
 (gnu packages guile)
 (gnu packages gl)
 (gnu packages nss)
 (gnu packages linux)
 (guix build-system trivial))

(define-public nomad
  (let ((commit "e8607c0e428faf0fa78fce651ee4ac0f17979c61"))
    (package
      (name "nomad")
      (version (git-version "0.0.4-alpha" "118" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/mrosset/nomad")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "1j4q0dj3h0micz8982vhzmis9nbmr4ylcnfqxhq7l8x5jphr1cjy"))))
      (build-system gnu-build-system)
      (native-inputs
       `(
	 ("emacs-htmlize", emacs-htmlize)
	 ))
      (inputs
       `(
	 ("pkg-config" ,pkg-config)
	 ("glib" ,glib)
	 ("autoconf" ,autoconf)
	 ("automake" ,automake)
	 ("gettext-minimal" ,gettext-minimal)
	 ("qtbase" ,qtbase)
	 ("qtwebchannel", qtwebchannel)
	 ("qtquickcontrols2" ,qtquickcontrols2)
	 ("qttools" ,qttools)
	 ))
      (propagated-inputs
       `(
	 ("guile" ,guile-2.2)
	 ("guile-readline" ,guile-readline)
	 ("qtwebkit" ,qtwebkit)
	 ("qtdeclarative" ,qtdeclarative)
	 ("qtquickcontrols" ,qtquickcontrols)
	 ("qtwebchannel" ,qtwebchannel)
	 ))
      (arguments
       `(#:phases (modify-phases %standard-phases
		    (add-after 'install-binaries 'wrap-binaries
		      (lambda* (#:key outputs inputs #:allow-other-keys)
			(let* ((out        (assoc-ref outputs "out"))
			       (qtwebkit   (assoc-ref inputs "qtwebkit"))
			       (netprocess (string-append qtwebkit "/lib/libexec/QtWebNetworkProcess"))
			       (webprocess (string-append qtwebkit "/lib/libexec/QtWebProcess"))
			       (bin        (string-append out "/bin"))
			       (exe        (string-append out "/bin/nomad")))
			  (symlink netprocess (string-append bin "/QtWebNetworkProcess"))
			  (symlink webprocess (string-append bin "/QtWebProcess"))
			  (wrap-program exe
			    ;; FIXME: qtwebkit packages uses /lib/qml
			    ;; instead of /lib/qt5/qml
			    `("QML2_IMPORT_PATH" ":" prefix
			      (,(string-append qtwebkit "/lib/qml:"))))
			  #t))))))
      (home-page "https://github.com/mrosset/nomad")
      (synopsis "An extensible web browser using GNU Guile.")
      (description "An extensible web browser.")
      (license license:gpl3+))))
