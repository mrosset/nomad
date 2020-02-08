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

## ACLOCAL_AMFLAG = -I m4

AM_CFLAGS = -Wall -Werror -Wdeclaration-after-statement

libguile_nomad_LTLIBRARIES = libguile-nomad.la
libguile_nomaddir = $(libexecdir)/nomad/

libguile_nomad_la_SOURCES =  scheme.c scheme.h
libguile_nomad_la_SOURCES +=  util.c util.h
libguile_nomad_la_SOURCES +=  resources.c

libguile_nomad_la_CFLAGS =  $(AM_CFLAGS)
libguile_nomad_la_CFLAGS += $(EMACSY_CFLAGS)
libguile_nomad_la_CFLAGS += $(GUILE_CFLAGS)
libguile_nomad_la_CFLAGS += $(GLIB_CFLAGS)
libguile_nomad_la_CFLAGS += $(GTK_CFLAGS)
libguile_nomad_la_CFLAGS += $(WEBKIT_CFLAGS)
libguile_nomad_la_CFLAGS += $(GTK_SOURCEVIEW_CFLAGS)


libguile_nomad_la_LIBADD =  $(GUILE_LIBS)
libguile_nomad_la_LIBADD += $(EMACSY_LIBS)
libguile_nomad_la_LIBADD += $(GLIB_LIBS)
libguile_nomad_la_LIBADD += $(GTK_LIBS)
libguile_nomad_la_LIBADD += $(WEBKIT_LIBS)
libguile_nomad_la_LIBADD += $(GTK_SOURCEVIEW_LIBS)

libguile_nomad_la_LDFLAGS = -avoid-version -no-undefined

G_RESOURCES = gresource.xml

BUILT_SOURCES = resources.c

EXTRA_DIST = $(G_RESOURCES)

CLEANFILES = $(BUILT_SOURCES)

.PHONY: format

resources.c: $(G_RESOURCES)
	echo    "GLIB       $@"
	@$(GLIB_COMPILE_R) $< --target=$@ --generate-source

format:
	 clang-format -i *.c *.h


-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --add-include-path=$(srcdir) --warn-all \
	--include Gtk-3.0 \
	--include GObject-2.0 \
	--include WebKit2-4.0 \
	--include $(GISOURCEVIEW)

INTROSPECTION_COMPILER_ARGS = --includedir=$(srcdir)

introspection_sources = scheme.h util.h

Nomad-1.0.gir: libguile-nomad.la
Nomad_1_0_INCLUDES =
Nomad_1_0_gir_CFLAGS = $(libguile_nomad_la_CFLAGS)
Nomad_1_0_gir_LIBS = libguile-nomad.la
Nomad_1_0_gir_FILES = $(introspection_sources)
INTROSPECTION_GIRS += Nomad-1.0.gir

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(libdir)/girepository-1.0
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES += $(gir_DATA) $(typelib_DATA)
