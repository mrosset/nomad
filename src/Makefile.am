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

.PHONY: format

G_RESOURCES = gresource.xml window.ui buffer.ui
EXTRA_DIST = $(G_RESOURCES)

nodist_nomad_SOURCES = resources.c

nomad_SOURCES =		\
	app.c		\
	app.h		\
	buffer.c	\
	buffer.h	\
	main.c		\
	minibuffer.c	\
	request.c	\
	request.h	\
	util.c		\
	util.h		\
	webkit.c	\
	webkit.h	\
	window.c	\
	window.h

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

AM_CFLAGS = -Wall -Werror -Wno-unused-function -Wdeclaration-after-statement	\
	-Wno-deprecated-declarations						\
	$(EMACSY_CFLAGS)							\
	$(GTK_CFLAGS)								\
	$(GUILE_CFLAGS)								\
	$(WEBKIT_CFLAGS)							\
	$(GTK_SOURCEVIEW_CFLAGS)						\
	-DNOMAD_SOCKET_FILE=\""/tmp/nomad-socket"\"				\
	-DNOMAD_GUILE_LOAD_PATH=\""$(moddir)"\"					\
	-DNOMAD_GUILE_LOAD_COMPILED_PATH=\""$(ccachedir)"\"			\
	-DNOMAD_WEB_EXTENSIONS_DIR=\""$(libexecdir)/nomad/"\"			\
	-DNOMAD_DATAROOT_DIR=\""$(datarootdir)/nomad/"\"

AM_LDFLAGS = -export-dynamic

nomad_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(EMACSY_LIBS) $(WEBKIT_LIBS) $(GTK_SOURCEVIEW_LIBS)


DOT_X_FILES = app.x buffer.x webkit.x window.x util.x minibuffer.x

BUILT_SOURCES = $(DOT_X_FILES) resources.c

CLEANFILES = $(DOT_X_FILES) resources.c

format:
	 clang-format -i *.c *.h

resources.c: $(G_RESOURCES)
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
.c.x:
	@echo "  SNARF" $@;
	@$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

DEVEL_SOCKET  = /tmp/nomad-devel

test: all
	-rm $(DEVEL_SOCKET)
	../pre-inst-env ./nomad --listen $(DEVEL_SOCKET)

test-url: all
	echo "test url"
	../pre-inst-env ./nomad --listen $(DEVEL_SOCKET) "https://bufio.org"
