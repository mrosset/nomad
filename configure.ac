PACKAGE=emacs
VERSION=0.1

AC_INIT([wemacs], [0.1], [mike.rosset@gmail.com])
AM_INIT_AUTOMAKE([-Wall -Werror gnu silent-rules])
AM_SILENT_RULES([yes])
AC_PROG_CC
AC_CONFIG_HEADERS([config.h])

#GUILE_TOOLS=guile2.2
GUILE_PROGS

# pkg-config modules
PKG_CHECK_MODULES([GUILE], [guile-2.2])
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0])
PKG_CHECK_MODULES([VTE], [vte-2.91])

AC_CONFIG_FILES([
 Makefile
 src/Makefile
])

AC_OUTPUT
