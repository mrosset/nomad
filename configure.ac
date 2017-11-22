PACKAGE=emacs
VERSION=0.1

# autoconf
AC_INIT([wemacs], [0.1], [mike.rosset@gmail.com])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AC_PROG_CC

# automake
AM_INIT_AUTOMAKE([-Wall gnu silent-rules])
AM_SILENT_RULES([yes])
AM_PROG_AR
LT_INIT

# Guile
GUILE_PROGS([2.2])
GUILE_PKG([2.2])
GUILE_FLAGS
GUILE_SITE_DIR([2.2])

# pkg-config modules
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0])
PKG_CHECK_MODULES([VTE], [vte-2.91])
PKG_CHECK_MODULES(WEB_EXTENSION, [webkit2gtk-web-extension-4.0])
PKG_CHECK_MODULES([GTK_SOURCEVIEW], [gtksourceview-3.0])
PKG_CHECK_MODULES(GLIB, [glib-2.0])

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 ext/Makefile
 scheme/Makefile
])

AC_OUTPUT
