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

PACKAGE=nomad
VERSION=0.0.2-alpha

# autoconf
AC_INIT([nomad], [0.0.2-alpha], [mike.rosset@gmail.com])
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
