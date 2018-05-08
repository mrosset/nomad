PACKAGE=nomad
VERSION=0.0.1-alpha

# autoconf
AC_INIT([nomad], [0.0.1-alpha], [mike.rosset@gmail.com])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AC_PROG_CC

# automake
AM_INIT_AUTOMAKE([-Wall gnu silent-rules])
AM_SILENT_RULES([yes])
AM_PROG_AR

# libtool
LT_INIT

# Guile
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

# pkg-config modules
PKG_CHECK_MODULES([GUILE], [guile-2.2])
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0])
PKG_CHECK_MODULES([VTE], [vte-2.91])
PKG_CHECK_MODULES(WEB_EXTENSION, [webkit2gtk-web-extension-4.0])
PKG_CHECK_MODULES([GTK_SOURCEVIEW], [gtksourceview-3.0])
PKG_CHECK_MODULES(GLIB, [glib-2.0])

AC_PATH_PROG(GUILE_SNARF, guile-snarf, no)
AC_SUBST(GUILE_SNARF)

if test "$GUILE_SNARF" = "no"; then
   AC_MSG_ERROR([program guile-snarf is required])
fi

AC_PATH_PROG(GUILD, guild, no)
AC_SUBST(GUILD)

if test "$GUILD" = "no"; then
   AC_MSG_ERROR([program guild is required])
fi

AC_PATH_PROG(GLIB_COMPILE_R, glib-compile-resources, no)
AC_SUBST(GLIB_COMPILE_R)

if test "$GLIB_COMPILE_R" = "no"; then
   AC_MSG_ERROR([program glib-compile-resources is required])
fi

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 ext/Makefile
 scheme/Makefile
])

AC_OUTPUT
