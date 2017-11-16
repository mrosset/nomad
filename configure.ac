PACKAGE=emacs
VERSION=0.1

AC_INIT([wemacs], [0.1], [mike.rosset@gmail.com])
AM_INIT_AUTOMAKE([-Wall -Werror gnu silent-rules])
AM_SILENT_RULES([yes])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])

AC_PROG_CC
AM_PROG_AR
LT_INIT

# pkg-config modules
PKG_CHECK_MODULES([GUILE], [guile-2.2])
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0])
PKG_CHECK_MODULES([VTE], [vte-2.91])
PKG_CHECK_MODULES(WEB_EXTENSION, [webkit2gtk-web-extension-4.0])
PKG_CHECK_MODULES(GLIB, [glib-2.0])

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 ext/Makefile
])

AC_OUTPUT
