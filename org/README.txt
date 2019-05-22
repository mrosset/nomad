Table of Contents
_________________

1. Installation
.. 1. With Guix
2. Run Nomad
.. 1. From the command line
.. 2. Keybinds
3. Extend Nomad
4. Nomad-hacker's setup
5. Build from source
.. 1. Run Nomad before you've installed Nomad
6. Hack Nomad in QML
.. 1. To implement a Method from Qtwebengine
7. FAQ
.. 1. Nomad doesn't start


1 Installation
==============

1.1 With Guix
~~~~~~~~~~~~~

  Nomad is not yet available in the official Guix, but we still try to
  provide substitutes to make installation easier.
  - Clone the Nomad source repository and checkout the current
    development branch.
    ,----
    | git clone https://git.savannah.gnu.org/git/nomad.git
    | cd nomad
    | git checkout feature-qt
    `----
  - Make sure Guix can find the nomad package, declared in
    `guix/gnu/packages/nomad.scm`.
    ,----
    | export GUIX_PACKAGE_PATH="$PWD/guix:$GUIX_PACKAGE_PATH"
    `----
  - If you'd like to to download pre-built binaries from the build
    server, you can authorize guix to use Nomad's build server.
    ,----
    | guix archive --authorize < contrib/guix/gx.bufio.org.pub
    `----
  - Then install Nomad while instructing guix to use substitutes from
    both, the official, and the Nomad build server.
    ,----
    | guix package -i nomad --substitute-urls="https://ci.guix.info \
    | https://gx.bufio.org"
    `----
  - If subsitutes are still not available you may have to use an older
    version of guix and retry.
    ,----
    | guix pull --commit=152030ffdf548527d32458c5f3fbc98700aa7e69
    `----


2 Run Nomad
===========

2.1 From the command line
~~~~~~~~~~~~~~~~~~~~~~~~~

  - Nomad doesn't yet support the full set of command line
    options. However, It can be run from a shell by typing
    ,----
    | nomad
    `----
  - If you want nomad to connect to an existing guile REPL use the
    following command line flag.
    ,----
    | nomad -c
    `----


2.2 Keybinds
~~~~~~~~~~~~

  - Nomad intends to support Emacs-like keybinds because it's design was
    inspired from Emacs. However there is no limitation on what
    Keybindings Nomad can support. Nomad has the concept of key-maps
    just like Emacs and users should be able to define their own
    keymaps.
  - At the top level you might want to use `M-x` (Meta/Alt X) to use the
    execute command menu and `C-x` (Ctrl X) for Ibuffer.
  - In a Webview buffer the `webview-mode-map` is active which contains
    the following keybinds.
  ---------------------------
   Key stroke  Command
  ---------------------------
   "C-b"       (next-buffer)
   "C-u"       (back)
   "C-m"       (forward)
   "C-n"       (scroll-down)
   "C-f"       (hints)
   "C-p"       (scroll-up)
   "C-r"       (reload)
   "C-x"       (kill-buffer)
  ---------------------------
  - In Nomad's Ibuffer menu the `ctrl-x-map` is active which has these
    keybindings.
  ---------------------------
   Key stroke  Command
  ---------------------------
   "b"         (next-buffer)
   "k"         (kill-buffer)
  ---------------------------


3 Extend Nomad
==============

  - You can customize nomad by writing your configurations to
    `~/.nomad`, which is just a scheme file!
  - Examples
    - (set! search-provider-format "<https://google.ca/search?q=~a>")
    - (define-command (help) "Browse to the Nomad README."  (browse
      "<file:///nomad/README.org>"))


4 Nomad-hacker's setup
======================

  - Startup Emacs and install Geiser
  - M-x geiser-connect-local RET /tmp/nomad-socket RET
  - Try (forward) (back) (browse "<https://gnu.org/software/guile>")
  - Nomad's extensions are defined in `$NOMAD_SOURCE/scheme/`, create
    new module for nomad as `scheme/nomad/<module-name>`.
  - Once you have tested it locally, you can submit a pull request to
    the Nomad source [repository].


[repository] <https://github.com/mrosset/nomad>


5 Build from source
===================

  - In Ubuntu 18.10, the the following snippet will install all required
    dependencies.
    ,----
    | sudo apt install -y \
    | guile-2.2-dev \
    | qtbase5-dev \
    | qtchooser \
    | qtwebengine5-dev \
    | qtquickcontrols2-5-dev \
    | qtbase5-dev-tools \
    | qml-module-qtquick2 \
    | qml-module-qtquick-controls \
    | qml-module-qtwebengine \
    | qml-module-qtwebchannel \
    | qml-module-qtquick-layouts \
    | libqtermwidget5-0-dev \
    `----

  - Using Guix, the following snippet will install all required
    dependencies in a temporary shell.
    ,----
    | export GUIX_PACKAGE_PATH="$NOMAD_SOURCE/guix:$GUIX_PACKAGE_PATH"
    | guix environment nomad
    `----

  - Then build the package with
    ,----
    | autoreconf -vif
    | ./configure
    | make
    `----


5.1 Run Nomad before you've installed Nomad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  - Once you've built Nomad without errors, to run Nomad from the local
    build.
  ,----
  | ./pre-inst-env ./qt/nomad
  `----


6 Hack Nomad in QML
===================

  - Nomad is hackable in Scheme. But you can also hack Nomad in
    Cpp/QML. In general, the aim of Nomad is to present you the user
    with enough programming capabilities that you don't have to resort
    to this step, but you may still wish to change some internal
    plumbing in Nomad or expose new functionality to Scheme, and we
    encourage you in this endeavour.
  - One design principles we can use as a of rule of thumb is, for every
    feature provided by Nomad in the standard library or in the
    application, user should be able to provide their replacement.
  - You can find features available in WebEngineView at [QtWebEngineView
    Features].


[QtWebEngineView Features]
<https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html#Feature-prop>

6.1 To implement a Method from Qtwebengine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  - You can add a procedure in Nomad Cpp modules that will become
    accessible from Scheme with `SCM_DEFINE` from the Guile's C library
    `libguile`.
    ,----
    | #include <libguile.h>
    `----
  - The `currentWebView` is an instance of Qt WebEngineView, it can
    support a function called `loadHtml`, which loads HTML into the
    WebView. But, we can also use it to load any type of data!
  - To make this facility available to Scheme, we'd like to define a
    procedure called `webview-load-string`, that can wrap up this
    functionality and make it available at Nomad's runtime.
  - The following `webview-load-string` procedure is one of the ways you
    can do it. Webview-load-string is converting it's argument, a Scheme
    object into a Qt object which can be understood by Qtwebengine.
    ,----
    | SCM_DEFINE (scm_webview_load_string, "webview-load-string", 1, 0, 0, (SCM html),
    |	  "Set's the current WebView to string HTML.")
    | {
    |   QMetaObject::invokeMethod(currentWebView(), "loadHtml",
    |     Qt::DirectConnection, Q_ARG(QString, (scm_to_qstring (html))));
    |
    |   return SCM_UNSPECIFIED;
    | }
    `----
  - Then, we invoke the loadHtml Method on this argument.. Although,
    this approach may work, the results can be less than robust.
  - It's good practice to give your function an unabbreivated and
    descriptive name and a documentation string explaining how to use
    the procedure.
  - To export this procedure use:
    ,----
    | scm_c_export ("webview-load-string");
    `----
  - Then you can compile Nomad and add this to your `$HOME/.nomad`
    ,----
    | (define-command (say-hello) (webview-load-string "Hello World"))
    `----
  - And in a running instance of Nomad, M-x say-hello.


7 FAQ
=====

7.1 Nomad doesn't start
~~~~~~~~~~~~~~~~~~~~~~~

  - you might have to delete the socket file `/tmp/nomad-socket`
