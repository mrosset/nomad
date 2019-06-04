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
AC_PROG_CXX
AM_PROG_AR
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MKDIR_P

# Guile
GUILE_PKG([2.2 2.0])
GUILE_PROGS
GUILE_FLAGS

# pkg-config modules
dnl PKG_CHECK_MODULES([GUILE], [guile-2.2])
PKG_CHECK_MODULES([QT_WIDGETS], [Qt5Widgets])
PKG_CHECK_MODULES([QT_QUICK_CONTROLS], [Qt5QuickControls2])

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

AC_CONFIG_FILES([
Makefile
qt/Makefile
scheme/Makefile
])

AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
