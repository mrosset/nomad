---
title: "Nomad Web Browser"
date: 2019-05-15
author: amar-m-singh
draft: false
template: article.pug
---

Today, we are looking at nomad web browser. It\'s an extensible web
browser. It uses a configuration file very much like .emacs. Nomad is
Free Software.

## Introduction

Nomad does not yet have a stable release but it can be installed using
guix or built from source.

Nomad is based on qtwebengine, which is itself based on google\'s
chromium. Nomad embeds Guile into qtwebengine, which then allows the
addition of new features, into Nomad at runtime and without writing any
C or compiling from source.

The browser\'s API is exposed to the user. Nomad\'s configuration in
\~/.nomad file is a plain-text file, actually a guile scheme file. Any
configuration or code inside this file is evaluated by Nomad at startup.

## Problems with Browsers

Why a new web browser? There are quite a few good web browsers out
there. Nomad is aspirationally the Emacs of web browsers. In nomad you
get a say in how you\'d like your browser to be. There is no company
deciding how your browser should be, rather you have the ability to
change it as you see fit.

## Emacs-like

Many of it\'s features are based on Emacs, and it aims to be just as fun
as emacs to hack and customise.

## Status

Nomad is usable but you want to keep another browser installed just in
case. It\'s hackable and feels very much like emacs. The nomad part of
the program is quite small, qtwebengine is enormous though.

## Run from Source

Let\'s try to install nomad. Since nomad is very much a WIP it\'s not
yet available through the usual \"apt get install\" but if you already
have an installation of guix, then this step should be a breeze. You can
ofcourse compile from source as well.

Download the source code for nomad from savannah, then change directory
to the nomad source.

``` {.shell}
git clone https://git.savannah.gnu.org/git/nomad.git
cd nomad
```

Nomad is being developed on the feature-qt branch so you want to
checkout that branch before you do anything.

``` {.shell}
git checkout feature-qt
```

The guix package for nomad is in guix/ so guix will need to be told how
to find it by exporting guix package path.

``` {.shell}
export GUIX_PACKAGE_PATH="$PWD/guix:$GUIX_PACKAGE_PATH"
```

Now we are almost ready to install the nomad package, if you\'d rather
download built binaries, you can authorise the subsitute server in guix.

``` {.shell}
guix archive --authorize < contrib/guix/gx.bufio.org.pub
```

Then install nomad.

``` {.shell}
guix package -i nomad -n # dry run
guix package -i nomad --substitute-urls="https://ci.guix.info https://gx.bufio.org"
```

If your system still insists on building nomad and qtwebengine it\'s
possible that your guix version is mismatched. Resolved by updating guix
to this commit version.

``` {.shell}
guix pull --commit=152030ffdf548527d32458c5f3fbc98700aa7e69
```

Once the build is done then you can start nomad by simply typing nomad.

## Run Nomad from the command line

-   Nomad doesn\'t yet support the full set of command line options.
    However, It can be run from a shell by typing

    ``` {.shell}
    nomad
    ```

-   If you want nomad to connect to an existing guile REPL use the
    following command line flag.

    ``` {.shell}
    nomad -c
    ```

## Keybinds

-   Nomad intends to support Emacs-like keybinds because it\'s design
    was inspired from Emacs. However there is no limitation on what
    Keybindings Nomad can support. Nomad has the concept of key-maps
    just like Emacs and users should be able to define their own
    keymaps.
-   At the top level you might want to use \`M-x\` (Meta/Alt X) to use
    the execute command menu and \`C-x\` (Ctrl X) for Ibuffer.
-   In a Webview buffer the \`webview-mode-map\` is active which
    contains the following keybinds.

  Key stroke   Command
  ------------ ---------------
  \"C-b\"      (next-buffer)
  \"C-u\"      (back)
  \"C-m\"      (forward)
  \"C-n\"      (scroll-down)
  \"C-f\"      (hints)
  \"C-p\"      (scroll-up)
  \"C-r\"      (reload)
  \"C-x\"      (kill-buffer)

-   In Nomad\'s Ibuffer menu the \`ctrl-x-map\` is active which has
    these keybindings.

  Key stroke   Command
  ------------ ---------------
  \"b\"        (next-buffer)
  \"k\"        (kill-buffer)

Mouse is supported in nomad, so you can click around and copy links.
Nomad also has a execute command menu like emacs.

This is where emacs comes in. You can live hack and control nomad using
Emacs and geiser, you\'ll need to have guile, emacs, and emacs-geiser
installed. Run geiser and ask it to connect to a local socket, nomad\'s
socket file should be present in /tmp as nomad-socket. Then you can run
any scheme code in nomad. For example you can tell nomad to go back or
forward on a page, change the search engine or extend the execute
command menu with your own commands.

``` {.scheme}
;; geiser-connect-local RET /tmp/nomad-socket RET
(browse "https://gnu.org/software/guile")
(set! search-provider-format "https://google.ca/search?q=~a")
```

Shutdown will close the repl. Then exit nomad.

If you want to save your customisations then you will write them to
\~/.nomad file. For example I want to use shroud password manager from
nomad. Since shroud is a guile library also, I\'ll just import it in
nomad configuration and define a new command like so.

``` {.scheme}
(use-modules
 (ice-9 regex)
 (ice-9 session)
 (shroud ui list)
 (shroud ui show)
 (shroud secret))
```

First I have a procedure to extract a list of password entries from a
GPG encrypted file.

``` {.scheme}
(define shroud-db
  (delay (shroud-list* "/home/nly/.shroud"
               (delay (load-secrets "/home/nly/.config/shroud/db-old.gpg")))))
```

Then, a prcedure to find matches in the list of entries.

``` {.scheme}
(define-public (pass-completion text)
  "Returns a list of matches in password list"
  (let ((completion '()))
    (map (lambda (s)
       (when (string-match text s)
         (set! completion (append completion (list s)))))
     (force shroud-db))
    completion))
```

And, another procedure that will copy the entry\'s password to the
clipboard.

``` {.scheme}
(define (show-entry entry)
  (shroud-show "/home/nly/.shroud"
           (delay (load-secrets "/home/nly/.config/shroud/db-old.gpg"))
           entry "--clipboard" "password"))
```

A convinient macro is provided by nomad called define command, this will
make my command available in M-x menu in Nomad.

``` {.scheme}
(define-command (find-entry entry)
  "Show password/secrets entry"
  (map show-entry (pass-completion entry)))
```

Now you can hack nomad in scheme, but you can also hack it in C.

In a later blog...

TODO: build from source ./pre-inst-env and friends TODO: hack Nomad in
Cpp
