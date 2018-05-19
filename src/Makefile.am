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

AUTOMAKE_OPTIONS = gnu

bin_PROGRAMS = nomad

.PHONY: test format

test:
	gcc  $(GUILE_CFLAGS) test.c -o $@
	./test

EXTRA_DIST = gresource.xml window.ui buffer.ui

nomad_SOURCES = main.c	\
	app.c		\
	app.h		\
	buffer.c	\
	buffer.h	\
	resources.c	\
	vte.c		\
	vte.h		\
	webkit.h	\
	webkit.c	\
	window.c	\
	window.h

AM_CFLAGS = -Wall -Werror -Wno-unused-function -Wdeclaration-after-statement \
	$(GTK_FLAGS) \
	$(GUILE_CFLAGS) \
	$(VTE_CFLAGS) \
	$(WEBKIT_CFLAGS) \
	$(GTK_SOURCEVIEW_CFLAGS) \
	-DNOMAD_WEB_EXTENSIONS_DIR=\""$(libexecdir)/nomad/"\"

AM_LDFLAGS = -export-dynamic

nomad_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(VTE_LIBS) $(WEBKIT_LIBS) $(GTK_SOURCEVIEW_LIBS)

DOT_X_FILES = app.x buffer.x webkit.x

BUILT_SOURCES = $(DOT_X_FILES) resources.c

CLEANFILES = $(DOT_X_FILES) resources.c

format:
	 clang-format -i *.c *.h

resources.c: gresource.xml window.ui buffer.ui
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts) $(AM_FLAGS)
