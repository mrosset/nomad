dnl configure.ac
dnl Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

dnl This file is part of Nomad

dnl Nomad is free software: you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl Nomad is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl See the GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License along
dnl with this program.  If not, see <http://www.gnu.org/licenses/>.

# autoconf
AC_INIT([nomad], [0.0.4-alpha], [mike.rosset@gmail.com])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])

# automake
AM_INIT_AUTOMAKE([-Wall gnu silent-rules color-tests])
AM_SILENT_RULES([yes])

# programs
AC_PROG_CC_C99
AM_PROG_AR
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LIBTOOL

# libtool
LT_INIT

# Guile
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
dnl GUILE_FLAGS
GUILE_SITE_DIR

AC_ARG_WITH([gtksourceview], AS_HELP_STRING([--with-gtksourceview],
			     [With gtksourceview version 3 or default 4]),
			     [],
			     [with_gtksourceview=4])

# pkg-config modules
PKG_CHECK_MODULES([GUILE], [guile-2.2])
PKG_CHECK_MODULES([EMACSY], [emacsy])
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0])
PKG_CHECK_MODULES(WEB_EXTENSION, [webkit2gtk-web-extension-4.0])

PKG_CHECK_MODULES(GLIB, [glib-2.0])
PKG_CHECK_MODULES(GUILE_LIB, [guile-lib-1.0])

AS_IF([test "x$with_gtksourceview" = "x4"], [
	    PKG_CHECK_MODULES([GTK_SOURCEVIEW], [gtksourceview-4])
])

AS_IF([test "x$with_gtksourceview" = "x3"], [
	    PKG_CHECK_MODULES([GTK_SOURCEVIEW], [gtksourceview-3.0])
])

dnl guile modules
GUILE_MODULE_AVAILABLE([guile_gcrypt], [(gcrypt hash)])
if test "$guile_gcrypt" = "no"; then
   AC_MSG_ERROR([guile-gcrypt is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([guile_readline], [(ice-9 readline)])
if test "$guile_readline" = "no"; then
   AC_MSG_ERROR([guile-readline is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([guile_lib], [(debugging assert)])
if test "$guile_lib" = "no"; then
   AC_MSG_ERROR([guile-lib is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([guile_emacsy], [(emacsy emacsy)])
if test "$guile_emacsy" = "no"; then
   AC_MSG_ERROR([guile emacsy is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([guile_curl], [(curl)])
if test "$guile_curl" = "no"; then
   AC_MSG_WARN([not using guile-curl])
fi

GUILE_MODULE_AVAILABLE([guile_shroud], [(shroud secret)])
if test "$guile_shroud" = "no"; then
   AC_MSG_ERROR([guile shroud is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([guile_gnutls], [(gnutls)])
if test "$guile_gnutls" = "no"; then
   AC_MSG_ERROR([guile gnutls is missing; please install it.])
fi

top_builddir_absolute=`pwd`
AC_SUBST(top_builddir_absolute)

top_srcdir_absolute=`(cd $srcdir && pwd)`
AC_SUBST(top_srcdir_absolute)

AC_DEFUN([GUILE_CONFIG_SCRIPT],[AC_CONFIG_FILES([$1],[chmod +x $1])])
GUILE_CONFIG_SCRIPT([check-guile])

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
guile/Makefile
src/Makefile
ext/Makefile
scheme/Makefile
])

AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT

AC_MSG_NOTICE([summary of build options:

  version:              ${VERSION}
  Host/Target system:   ${host}
  Build system:         ${build}
  Install prefix:       ${prefix}
  Compiler:             ${CC}
  Valgrind:             $opt_valgrind_tests ${VALGRIND}
  CFlags:               ${CFLAGS}
  Library types:        Shared=${enable_shared}, Static=${enable_static}
])

AC_MSG_NOTICE([summary of GTK libraries:

  GtkSourceView:       ${with_gtksourceview}
])

AC_MSG_NOTICE([summary of guile libraries:

  Emacsy:              ${guile_emacsy}
  GnuTLS:              ${guile_gnutls}
  Guile gcrypt:        ${guile_gcrypt}
  Guile Library:       ${guile_lib}
  Readline:            ${guile_readline}
  Curl:                ${guile_curl}
  Shroud:              ${guile_shroud}
])
