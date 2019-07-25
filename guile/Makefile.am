## Makefile.am
## Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

## This file is part of Nomad

## Nomad is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## Nomad is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this program.  If not, see <http://www.gnu.org/licenses/>.

##ACLOCAL_AMFLAG = -I m4

AM_CFLAGS = -Wall -Werror

libguile_nomad_LTLIBRARIES = libguile-nomad.la
libguile_nomaddir = $(libexecdir)/nomad/
libguile_nomad_la_SOURCES = extension.c webkit.c frame.c
libguile_nomad_la_CFLAGS = $(AM_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS) $(WEBKIT_CFLAGS)
libguile_nomad_la_LIBADD = $(GUILE_LIBS) $(GLIB_LIBS) $(GTK_LIBS) $(WEBKIT_LIBS)
libguile_nomad_la_LDFLAGS = -module -avoid-version -no-undefined

DOT_X_FILES = extension.x webkit.x frame.x

BUILT_SOURCES = $(DOT_X_FILES)

CLEANFILES = $(DOT_X_FILES)

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(AM_CFLAGS) $(libguile_nomad_la_CFLAGS)

SUFFIXES = .x

.c.x:
	@echo "  SNARF" $@;
	@$(GUILE_SNARF) -o $@ $< $(snarfcppopts)
