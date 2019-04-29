(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages qt)
 (gnu packages pkg-config)
 (gnu packages guile)
 (gnu packages glib)
 (gnu packages gl)
 (gnu packages gnupg)
 (gnu packages gtk)
 (gnu packages gnome)
 (gnu packages gettext)
 (gnu packages autotools)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages cups)
 (gnu packages compression)
 (gnu packages databases)
 (gnu packages icu4c)
 (gnu packages image)
 (gnu packages re2c)
 (gnu packages bison)
 (gnu packages flex)
 (gnu packages fontutils)
 (gnu packages freedesktop)
 (gnu packages linux)
 (gnu packages maths)
 (gnu packages gperf)
 (gnu packages perl)
 (gnu packages pcre)
 (gnu packages pulseaudio)
 (gnu packages pciutils)
 (gnu packages python)
 (gnu packages ruby)
 (gnu packages sqlite)
 (gnu packages ninja)
 (gnu packages tls)
 (gnu packages vulkan)
 (gnu packages nss)
 (gnu packages xml)
 (gnu packages xorg)
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

(define-public qtbase
  (package
    (name "qtbase")
    (version "5.12.3")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://download.qt.io/official_releases/qt/"
				  (version-major+minor version) "/" version
				  "/submodules/" name "-everywhere-src-"
				  version ".tar.xz"))
	      (sha256
	       (base32
		"00g7bzfgxc1z1ghyh6ibmrlhh5i52inqfyynxvmkyl7p5s2xipzx"))
	      ;; Use TZDIR to avoid depending on package "tzdata".
;;	      (patches (search-patches "qtbase-use-TZDIR.patch"
;;				       "qtbase-old-kernel.patch"))
	      (modules '((guix build utils)))
	      (snippet
	       ;; corelib uses bundled harfbuzz, md4, md5, sha3
	       '(begin
		  (with-directory-excursion "src/3rdparty"
		    (for-each delete-file-recursively
			      (list "double-conversion" "freetype" "harfbuzz-ng"
				    "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
				    "zlib"))
		    #t)))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)
       ("which" ,(@ (gnu packages base) which))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("double-conversion" ,double-conversion)
       ("eudev" ,eudev)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mtdev" ,mtdev)
       ("mariadb" ,mariadb)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("pcre2" ,pcre2)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite-with-column-metadata)
       ("unixodbc" ,unixodbc)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("vulkan-headers" ,vulkan-headers)
       ("ruby" ,ruby)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-after 'configure 'patch-bin-sh
	   (lambda _
	     (substitute* '("config.status"
			    "configure"
			    "mkspecs/features/qt_functions.prf"
			    "qmake/library/qmakebuiltins.cpp")
	       (("/bin/sh") (which "sh")))
	     #t))
	 (replace 'configure
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let ((out (assoc-ref outputs "out")))
	       (substitute* "configure"
		 (("/bin/pwd") (which "pwd")))
	       (substitute* "src/corelib/global/global.pri"
		 (("/bin/ls") (which "ls")))
	       ;; The configuration files for other Qt5 packages are searched
	       ;; through a call to "find_package" in Qt5Config.cmake, which
	       ;; disables the use of CMAKE_PREFIX_PATH via the parameter
	       ;; "NO_DEFAULT_PATH". Re-enable it so that the different
	       ;; components can be installed in different places.
	       (substitute* (find-files "." ".*\\.cmake")
		 (("NO_DEFAULT_PATH") ""))
	       ;; do not pass "--enable-fast-install", which makes the
	       ;; configure process fail
	       (invoke
		"./configure"
		"-verbose"
		"-prefix" out
		"-docdir" (string-append out "/share/doc/qt5")
		"-headerdir" (string-append out "/include/qt5")
		"-archdatadir" (string-append out "/lib/qt5")
		"-datadir" (string-append out "/share/qt5")
		"-examplesdir" (string-append
				out "/share/doc/qt5/examples")
		"-opensource"
		"-confirm-license"

		;; These features require higher versions of Linux than the
		;; minimum version of the glibc.  See
		;; src/corelib/global/minimum-linux_p.h.  By disabling these
		;; features Qt5 applications can be used on the oldest
		;; kernels that the glibc supports, including the RHEL6
		;; (2.6.32) and RHEL7 (3.10) kernels.
		"-no-feature-getentropy" ; requires Linux 3.17

		;; Do not build examples; if desired, these could go
		;; into a separate output, but for the time being, we
		;; prefer to save the space and build time.
		"-no-compile-examples"
		;; Most "-system-..." are automatic, but some use
		;; the bundled copy by default.
		"-system-sqlite"
		"-system-harfbuzz"
		"-system-pcre"
		;; explicitly link with openssl instead of dlopening it
		"-openssl-linked"
		;; explicitly link with dbus instead of dlopening it
		"-dbus-linked"
		;; don't use the precompiled headers
		"-no-pch"
		;; drop special machine instructions that do not have
		;; runtime detection
		,@(if (string-prefix? "x86_64"
				      (or (%current-target-system)
					  (%current-system)))
		      '()
		      '("-no-sse2"))
		"-no-mips_dsp"
		"-no-mips_dspr2"))))
	 (add-after 'install 'patch-mkspecs
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (archdata (string-append out "/lib/qt5"))
		    (mkspecs (string-append archdata "/mkspecs"))
		    (qt_config.prf (string-append
				    mkspecs "/features/qt_config.prf")))
	       ;; For each Qt module, let `qmake' uses search paths in the
	       ;; module directory instead of all in QT_INSTALL_PREFIX.
	       (substitute* qt_config.prf
		 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
		  "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt5))")
		 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
		  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
		 (("\\$\\$\\[QT_HOST_LIBS\\]")
		  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
		 (("\\$\\$\\[QT_INSTALL_BINS\\]")
		  "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

	       ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
	       (substitute* (string-append mkspecs "/features/qt_functions.prf")
		 (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
		  "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

	       ;; Resolve qmake spec files within qtbase by absolute paths.
	       (substitute*
		   (map (lambda (file)
			  (string-append mkspecs "/features/" file))
			'("device_config.prf" "moc.prf" "qt_build_config.prf"
			  "qt_config.prf" "winrt/package_manifest.prf"))
		 (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
		 (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata))
	       #t)))
	 (add-after 'unpack 'patch-paths
	   ;; Use the absolute paths for dynamically loaded libs, otherwise
	   ;; the lib will be searched in LD_LIBRARY_PATH which typically is
	   ;; not set in guix.
	   (lambda* (#:key inputs #:allow-other-keys)
	     ;; libresolve
	     (let ((glibc (assoc-ref inputs ,(if (%current-target-system)
						 "cross-libc" "libc"))))
	       (substitute* '("src/network/kernel/qdnslookup_unix.cpp"
			      "src/network/kernel/qhostinfo_unix.cpp")
		 (("^\\s*(lib.setFileName\\(QLatin1String\\(\")(resolv\"\\)\\);)" _ a b)
		  (string-append a glibc "/lib/lib" b))))
	     ;; X11/locale (compose path)
	     (substitute* "src/plugins/platforminputcontexts/compose/generator/qtablegenerator.cpp"
	       ;; Don't search in /usr/…/X11/locale, …
	       (("^\\s*m_possibleLocations.append\\(QStringLiteral\\(\"/usr/.*/X11/locale\"\\)\\);" line)
		(string-append "// " line))
	       ;; … but use libx11's path
	       (("^\\s*(m_possibleLocations.append\\(QStringLiteral\\()X11_PREFIX \"(/.*/X11/locale\"\\)\\);)" _ a b)
		(string-append a "\"" (assoc-ref inputs "libx11") b)))
	     ;; libGL
	     (substitute* "src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp"
	       (("^\\s*(QLibrary lib\\(QLatin1String\\(\")(GL\"\\)\\);)" _ a b)
		(string-append a (assoc-ref inputs "mesa") "/lib/lib" b)))
	     ;; libXcursor
	     (substitute* "src/plugins/platforms/xcb/qxcbcursor.cpp"
	       (("^\\s*(QLibrary xcursorLib\\(QLatin1String\\(\")(Xcursor\"\\), 1\\);)" _ a b)
		(string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b))
	       (("^\\s*(xcursorLib.setFileName\\(QLatin1String\\(\")(Xcursor\"\\)\\);)" _ a b)
		(string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b)))
	     #t)))))
    (native-search-paths
     (list (search-path-specification
	    (variable "QMAKEPATH")
	    (files '("lib/qt5")))
	   (search-path-specification
	    (variable "QML2_IMPORT_PATH")
	    (files '("lib/qt5/qml")))
	   (search-path-specification
	    (variable "QT_PLUGIN_PATH")
	    (files '("lib/qt5/plugins")))
	   (search-path-specification
	    (variable "XDG_DATA_DIRS")
	    (files '("share")))
	   (search-path-specification
	    (variable "XDG_CONFIG_DIRS")
	    (files '("etc/xdg")))))
    (home-page "https://www.qt.io/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public qtwebengine
  (package
    (name "qtwebengine")
    (version "5.12.3")
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
	 "05s3vkili783idpsz4rrd877n7s1zrvp1fwkv4zhzakm5p0vmwrz"))))
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
  (package
    (name "nomad")
    (version "8b01a15d131a68a6945e2f9f5bc56b7eec26ea2d")
    ;; feature-qt branch
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/mrosset/nomad")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"03szz9vz8mci8ay3j5jysd3dmjl4jg1s26mnar1zvi1dlm4dfnpc"))))
    (build-system gnu-build-system)
    (inputs
     `(
       ("pkg-config" ,pkg-config)
       ("glib" ,glib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("guile-2.2" ,guile-2.2)
       ("guile-readline" ,guile-readline)
       ("qtdeclarative" ,qtdeclarative)
       ("qtbase" ,qtbase)
       ("qtwebengine" ,qtwebengine)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qttools" ,qttools) ;; qtbase5-dev-tools

       ;; Current Status: "./configure" phase: Error
       ;; checking for Qt5Widgets... yes
       ;; checking for Qt5WebEngine... no
       ;; configure: error: Package requirements (Qt5WebEngine) were not met:
       ;; No package 'Qt5WebEngine' found

       ;; ("qtquickcontrols" ,qtquickcontrols)
       ;; ("qtwebengine5",qtwebengine5) ;; possibly missing from guix?
       ;; ("qml-module-qtquick2",qml-module-qtquick2)
       ;; maybe qtquickcontrols also
       ;; ("qml-module-qtwebengine",qml-module-qtwebengine)
       ;; ("qml-module-qtquick-layouts",qml-module-qtquick-layouts)
       ;; ("libqtermwidget5-0",libqtermwidget5-0)
       ;; ("qtwayland" ,qtwayland)?? ^libqtermwidget
       ))
    (home-page "https://github.com/mrosset/nomad")
    (synopsis "An extensible web browser using Gnu Guile and QT.")
    (description "An extensible web browser.")
    (license license:gpl3+)))

nomad
