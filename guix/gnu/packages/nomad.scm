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
 (gnu packages xorg)
 (gnu packages gl)
 (gnu packages gnupg)
 (gnu packages pciutils)
 (gnu packages nss)
 (gnu packages gperf)
 (gnu packages cups)
 (gnu packages pulseaudio)
 (gnu packages gnome)
 (gnu packages linux)
 (gnu packages fontutils)
 (gnu packages gtk)
 (gnu packages re2c)
 (gnu packages perl)
 (gnu packages python)
 (gnu packages flex)
 (gnu packages bison)
 (gnu packages ruby)
 (gnu packages ninja)
 (gnu packages xdisorg)
 (gnu packages compression)
 (gnu packages vulkan)
 (guix build-system trivial))

(define-public qtwebengine
  (package
    (name "qtwebengine")
    (version "5.11.3")
    (source
     (origin
       (method url-fetch)
       (uri
	(string-append "https://download.qt.io/official_releases/qt/"
		       (substring version 0 4)
		       "/" version "/submodules/"
		       (string-append name "-everywhere-src-" version)
		       ".tar.xz"))
       (sha256
	(base32
	 "1zmqsdais85cdfh2jh8h4a5jcamp1mzdk3vgqm6xnldqf6nrxd2v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(
       ("perl" ,perl)
       ("python-2" ,python-2)
       ("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)
       ("ruby" ,ruby)
       ("ninja" ,ninja)
       ))
    (inputs
     `(
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("libxkbcommon" ,libxkbcommon)
       ("libx11" ,libx11)
       ("libxrender" ,libxrender)
       ("libxi" ,libxi)
       ;; OpenGL
       ("mesa" ,mesa)
       ;; qt web engine
       ("libgcrypt" ,libgcrypt)
       ("pciutils" ,pciutils)
       ("nss" ,nss)
       ("libxtst" ,libxtst)
       ("gperf" ,gperf)
       ("cups-minimal" ,cups-minimal)
       ("pulseaudio" ,pulseaudio)
       ("udev" ,eudev)
       ;; systemd-devel? no systemd on guix
       ("libcap" ,libcap)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("libxrandr" ,libxrandr)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("fontconfig" ,fontconfig)
       ("qtwebchannel" ,qtwebchannel)
       ("atk" ,atk)
       ("qtmultimedia" ,qtmultimedia)
       ("re2c" ,re2c)
       ))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-before 'configure 'configure-qmake
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (qtbase (assoc-ref inputs "qtbase"))
		    (tmpdir (string-append (getenv "TMPDIR")))
		    (qmake (string-append tmpdir "/qmake"))
		    (qt.conf (string-append tmpdir "/qt.conf")))
	       ;; Use qmake with a customized qt.conf to override install
	       ;; paths to $out.
	       (symlink (which "qmake") qmake)
	       (setenv "CC" "gcc")
	       (setenv "PATH" (string-append tmpdir ":" (getenv "PATH")))
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
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=lib/qt5/libexec
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5
" out out qtbase)))
	       #t)))
	 (replace 'configure
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     ;; Valid QT_BUILD_PARTS variables are:
	     ;; libs tools tests examples demos docs translations
	     (invoke "qmake" "QT_BUILD_PARTS = libs tools")))
	 (add-before 'check 'set-display
	   (lambda _
	     ;; make Qt render "offscreen", required for tests
	     (setenv "QT_QPA_PLATFORM" "offscreen")
	     #t))
	 (add-after 'install 'copy-icu
	   (lambda* (#:key inputs outputs
			   #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (resources (string-append out "/share/qt5/resources/"))
		    (libexec (string-append out "/lib/qt5/libexec/"))
		    (copy-resource (lambda(x)
				     (copy-file (string-append resources x)
						(string-append libexec x)))))
	       (copy-resource "icudtl.dat")
	       (copy-resource "qtwebengine_resources.pak")
	       (copy-resource "qtwebengine_resources_200p.pak")
	       (copy-resource "qtwebengine_resources_100p.pak")
	       (copy-resource "qtwebengine_devtools_resources.pak"))
	     #t))
)))
       (home-page "https://www.qt.io")
       (synopsis "Qt5WebEngine")
       (description "Qt5WebEngine for nomad. Provides support for web
applications using the Chromium browser project.")
       (license
	(package-license qt))))

(define-public qt-with-webengine
  (package
    (name "qt-with-webengine")
    (version (package-version qtbase))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
		   (use-modules (ice-9 match)
				(guix build union))
		   (match %build-inputs
		     (((names . directories) ...)
		      (union-build (assoc-ref %outputs "out")
				   directories)
		      #t))
		   (let ((out (assoc-ref %outputs "out")))
		   (with-output-to-file (string-append out "/bin/qt.conf")
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
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=lib/qt5/libexec
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib
" out out))))
)))
    (inputs `(("qtbase", qtbase)
	      ("qtwebengine" ,qtwebengine)
	      ("qttools", qttools)
	      ("zlib", zlib)))
    (synopsis "Union qtbase and webengine")
    (description
     "Union of qtbase and webengine")
    (home-page (package-home-page qtbase))
    (license (package-license qtbase))))

(define-public nomad
  ;; feature-qt branch
  (let ((commit "bc2807a16dbc17a766d3f920d51ad9a57dbfec0f"))
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
		  "0sdglj66b9n6l3hrywb5xwmg0cjk0pjljiw3y08npkr46jrjvrpb"))))
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
		    (add-after 'install 'symlink-stuff
		      (lambda* (#:key inputs outputs
				#:allow-other-keys)
			(let* ((out (assoc-ref outputs "out"))
			       (qtwebengine (assoc-ref inputs
						       "qtwebengine"))
			       (qtwebengine-locales (string-append
						     qtwebengine
						     "/lib/qt5/libexec/qtwebengine_locales"))
			       (qtwebengine-resources (string-append
						       qtwebengine
						       "/share/qt5/resources/"))
			       (qtwebengine-libexec (string-append
						     qtwebengine "/lib/qt5/libexec/QtWebEngineProcess"))
			       (qt-resource/ (lambda (x) (string-append
							  qtwebengine-resources
							  x)))
			       (out-bin/ (lambda (x) (string-append out "/bin/"
								    x)))
			       (link-resource (lambda (x) (symlink (qt-resource/ x)
								   (out-bin/ x))))
			       (copy-resource (lambda (x)
						(copy-file (qt-resource/ x) (out-bin/ x)))))
			  (symlink qtwebengine-locales (out-bin/
							"qtwebengine_locales"))
			  (link-resource "icudtl.dat")
			  (link-resource "qtwebengine_resources.pak")
			  (link-resource "qtwebengine_resources_200p.pak")
			  (link-resource "qtwebengine_resources_100p.pak")
			  (link-resource "qtwebengine_devtools_resources.pak"))
			#t))
		    (add-after 'install-binaries 'wrap-binaries
		      (lambda* (#:key outputs inputs #:allow-other-keys)
			;; Curl and libvorbis need to be wrapped so that we get
			;; sound and networking.
			(let* ((out  (assoc-ref outputs "out"))
			       (exe  (string-append out "/bin/nomad"))
			       (lib  (string-append out "/lib"))
			       (mesa (assoc-ref inputs "mesa"))
			       (nss  (assoc-ref inputs "nss"))
			       (udev (assoc-ref inputs "udev")))
			  (wrap-program exe
			    ;; TODO: Get these in RUNPATH.
			    `("LD_LIBRARY_PATH" ":" prefix
			      (,(string-append lib ":" nss "/lib/nss:" mesa "/lib:"
					       udev "/lib"))))
			  #t))))))
      (home-page "https://github.com/mrosset/nomad")
      (synopsis "An extensible web browser using Gnu Guile and QT.")
      (description "An extensible web browser.")
      (license license:gpl3+))))
