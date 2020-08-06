Table of Contents
_________________

1 Installation
.. 1.1 Guix
..... 1.1.1 Installing with Guix
..... 1.1.2 Developing with Git
2 Run Nomad
.. 2.1 From the command line
.. 2.2 Keybinds
3 Extend Nomad
4 Build from source
.. 4.1 Run Nomad before you've installed Nomad


1 Installation
==============

1.1 Guix
~~~~~~~~

1.1.1 Installing with Guix
--------------------------

  ,----
  | guix install nomad
  `----


1.1.2 Developing with Git
-------------------------

* 1.1.2.1 Clone the Nomad source repository

  ,----
  | git clone https://git.savannah.gnu.org/git/nomad.git
  | cd nomad
  `----


* 1.1.2.2 Create a Nomad environment

  ,----
  | guix environment -L ./guix nomad-git
  `----


* 1.1.2.3 Bootstrap Autotools

  ,----
  | ./autogen.sh
  `----


* 1.1.2.4 Build and Run development Nomad

  ,----
  | ./configure
  | make -j(nproc)
  | make run
  `----


2 Run Nomad
===========

2.1 From the command line
~~~~~~~~~~~~~~~~~~~~~~~~~

  Nomad doesn't yet support the full set of command line
  options. However, It can be run from a shell by typing

  ,----
  | nomad
  `----


2.2 Keybinds
~~~~~~~~~~~~

  Nomad intends to support Emacs-like keybinds because it's design was
  inspired from Emacs. However there is no limitation on what
  Keybindings Nomad can support. Nomad has the concept of key-maps just
  like Emacs and users should be able to define their own keymaps.

  At the top level you might want to use `M-x` (Meta/Alt X) to use the
  execute command menu.

  In a Webview buffer the `webview-mode-map` is active which contains
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
  ---------------------------

  In Nomad's Ibuffer menu the `ctrl-x-map` is active which has these
  keybindings.

  ---------------------------
   Key stroke  Command       
  ---------------------------
   "b"         (next-buffer) 
   "k"         (kill-buffer) 
  ---------------------------


3 Extend Nomad
==============

  You can customize nomad by writing your configurations to `~/.nomad`,
  which is just a scheme file!

  Examples
  ,----
  | (set! search-provider-format "https://google.ca/search?q=~a")
  `----


4 Build from source
===================

  In Ubuntu 19.04, the the following snippet will install most
  dependencies.
  ,----
  | sudo apt install -y \
  | guile-2.2 \
  | guile-2.2-dev \
  | guile-library \
  | libgtk-3-dev \
  | libwebkit2gtk-4.0-dev \
  | libgtksourceview-4-dev
  `----

  Using Guix, the following snippet will install all required
  dependencies in a temporary shell.
  ,----
  | guix environment -l ./guix.scm
  `----

  Then build the package with
  ,----
  | autoreconf -vif
  | ./configure
  | make
  `----


4.1 Run Nomad before you've installed Nomad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Once you've built Nomad without errors, to run Nomad from the local
  build.
  ,----
  | make test
  `----
