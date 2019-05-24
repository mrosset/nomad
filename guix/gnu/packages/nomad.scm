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
 (gnu packages guile)
 (gnu packages gl)
 (gnu packages nss)
 (gnu packages linux)
 (guix build-system trivial))

(define-public nomad
  ;; feature-qt branch
  (let ((commit "b372b3b984dc241760b578e8e3b40b5be6e2a0f2"))
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
		  "144kr50w9kdlgamv3rjny70f7hk40cfq5f0n0fws4lls56x3w41a"))))
      (build-system gnu-build-system)
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
	 ("nss" ,nss)
	 ("mesa" , mesa)
	 ("udev", eudev)
	 ))
      (propagated-inputs
       `(
	 ("guile" ,guile-2.2)
	 ("guile-readline" ,guile-readline)
	 ("qtwebengine" ,qtwebengine)
	 ("qtdeclarative" ,qtdeclarative)
	 ("qtquickcontrols" ,qtquickcontrols)
	 ("qtwebchannel" ,qtwebchannel)
	 ))
      (arguments
       `(#:phases (modify-phases %standard-phases
		    (add-after 'install-binaries 'wrap-binaries
		      (lambda* (#:key outputs inputs #:allow-other-keys)
			(let* ((out       (assoc-ref outputs "out"))
			       (exe       (string-append out "/bin/nomad"))
			       (lib       (string-append out "/lib"))
			       (mesa      (assoc-ref inputs "mesa"))
			       (nss       (assoc-ref inputs "nss"))
			       (udev      (assoc-ref inputs "udev"))
			       (qtbase    (assoc-ref inputs "qtbase"))
			       (qtwebengine (assoc-ref inputs "qtwebengine"))
			       (qt.conf   (string-append out "/bin/qt.conf")))
			  (wrap-program exe
			    ;; TODO: Get these in RUNPATH.
			    `("LD_LIBRARY_PATH" ":" prefix
			      (,(string-append lib ":" nss "/lib/nss:" mesa "/lib:"
					       udev "/lib")))
			    `("QTWEBENGINEPROCESS_PATH" ":" prefix (,(string-append qtwebengine "/lib/qt5/libexec/QtWebEngineProcess"))))
			  (with-output-to-file qt.conf
			    (lambda ()
			      (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=~a/lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib
" qtwebengine qtbase qtwebengine)))
			  #t))))))
      (home-page "https://github.com/mrosset/nomad")
      (synopsis "An extensible web browser using Gnu Guile and QT.")
      (description "An extensible web browser.")
      (license license:gpl3+))))
