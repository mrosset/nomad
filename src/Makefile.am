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

## bin_PROGRAMS = nomad

bin_SCRIPTS = nomad

CLEANFILES = resources.c

G_RESOURCES = gresource.xml
EXTRA_DIST = $(G_RESOURCES) nomad.in

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g'

nomad: nomad.in Makefile
	$(do_subst) < $(srcdir)/nomad.in > $@
	chmod +x nomad

nodist_nomad_SOURCES = resources.c

nomad_SOURCES =	main.c

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

AM_CFLAGS = -Wall -Werror -Wno-unused-function -Wdeclaration-after-statement	\
	-Wno-deprecated-declarations

nomad_CFLAGS= $(EMACSY_CFLAGS)							\
	$(GTK_CFLAGS)								\
	$(GUILE_CFLAGS)								\
	$(WEBKIT_CFLAGS)							\
	$(GTK_SOURCEVIEW_CFLAGS)						\
	-DNOMAD_GUILE_LOAD_PATH=\""$(moddir)"\"					\
	-DNOMAD_GUILE_LOAD_COMPILED_PATH=\""$(ccachedir)"\"			\
	-DNOMAD_WEB_EXTENSIONS_DIR=\""$(libexecdir)/nomad/"\"			\
	-DNOMAD_DATAROOT_DIR=\""$(datarootdir)/nomad/"\"

AM_LDFLAGS = -export-dynamic -lguile-nomad

nomad_LDADD = -L../guile/.libs $(GTK_LIBS) $(GUILE_LIBS) $(EMACSY_LIBS) $(WEBKIT_LIBS) $(GTK_SOURCEVIEW_LIBS)

BUILT_SOURCES = resources.c

.PHONY: format

format:
	 clang-format -i *.c *.h

resources.c: $(G_RESOURCES)
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source

test: all
	$(top_builddir)/pre-inst-env ./nomad --gapplication-app-id "org.devel.nomad"

test-url: all
	echo "test url"
	../pre-inst-env ./nomad "https://bufio.org" --listen $(DEVEL_SOCKET)
