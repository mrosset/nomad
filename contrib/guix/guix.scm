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
 (gnu packages xdisorg))

(define-public qt-with-web-engine
  (package
    (inherit qt)
    (name "qt-with-web-engine")
    (version "5.11.3")
    (source (origin
	      (method url-fetch)
	      (uri
	       (string-append
		"http://download.qt.io/official_releases/qt/"
		(version-major+minor version)
		"/" version
		"/single/qt-everywhere-src-"
		version ".tar.xz"))
	      (sha256
	       (base32
		"0kgzy32s1fr22fxxfhcyncfryb3qxrznlr737r4y5khk4xj1g545"))))))

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
       ;; qtbase
       ("qtbase" ,qtbase)

       ;; Missing module: qml quick
       ("qtdeclarative" ,qtdeclarative)

       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ;; only if more xcb-* depends are required, add them
       ("libxkbcommon" ,libxkbcommon)
       ("libx11" ,libx11)
       ("libxrender" ,libxrender)
       ("libxi" ,libxi)
       ;; OpenGL
       ("mesa" ,mesa)
       ;; Qt Quick 2; missing

       ;; Accessibility
       ;; ("python-atspi" ,python-atspi)
       ;; ("dbus" ,dbus)

       ;; Qt webkit; Optional?
       ;; ("qtwebkit" ,qtwebkit)

       ;; qt web engine
       ("libgcrypt" ,libgcrypt)
       ("pciutils" ,pciutils)
       ("nss" ,nss)
       ("libxtst" ,libxtst)
       ("gperf" ,gperf)
       ("cups-minimal" ,cups-minimal)
       ("pulseaudio" ,pulseaudio)
       ("libgudev" ,libgudev)
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
       ;; qt multimedia
       ("qtmultimedia" ,qtmultimedia)
       ;; qdoc documentation generator tool

       ;; warning: A compatible version of re2c (>= 0.11.3) was not
       ;; found; changes to src/*.in.cc will not affect your build.
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
Translations=share/qt5/translations
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
	     #t)))))

    (home-page "https://www.qt.io")
    (synopsis "Qt5WebEngine")
    (description "Qt5WebEngine for nomad. Provides support for web
applications using the Chromium browser project.")
    (license
     (package-license qt))))

(define-public nomad
  ;; feature-qt branch
  (let ((commit "56bc7e94ed43091d641752d7b1e4af6e373913cc"))
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
		  "12xrpi1qxj3150ch6i15vxi8i5gb66q279f4aa4jsryaissyckh2"))))
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

         ;; ("qtquickcontrols" ,qtquickcontrols)
         ;; ("qtwebengine5",qtwebengine5) ;; possibly missing from guix?
         ;; ("qml-module-qtquick2",qml-module-qtquick2)
         ;; maybe qtquickcontrols also
         ;; ("qml-module-qtwebengine",qml-module-qtwebengine)
         ;; ("qml-module-qtquick-layouts",qml-module-qtquick-layouts)
         ;; ("libqtermwidget5-0",libqtermwidget5-0)
         ;; ("qtwayland" ,qtwayland)?? ^libqtermwidget
         ))
      (propagated-inputs
       `(
         ("guile-2.2" ,guile-2.2)
         ("guile-readline" ,guile-readline)
         ("qtwebengine" ,qtwebengine)
         ("qtdeclarative" ,qtdeclarative)
         ("qtquickcontrols" ,qtquickcontrols)
         ("qtwebchannel" ,qtwebchannel)
         ))
      (home-page "https://github.com/mrosset/nomad")
      (synopsis "An extensible web browser using Gnu Guile and QT.")
      (description "An extensible web browser.")
      (license license:gpl3+))))

nomad
