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

libguile_nomad_la_SOURCES =  app.c app.h
libguile_nomad_la_SOURCES += frame.c frame.h
libguile_nomad_la_SOURCES += minibuffer.c minibuffer.h
libguile_nomad_la_SOURCES += resources.c
libguile_nomad_la_SOURCES += text.c text.h
libguile_nomad_la_SOURCES += util.c util.h
libguile_nomad_la_SOURCES += webkit.c webkit.h
libguile_nomad_la_SOURCES += webkitsettings.c webkitsettings.h
libguile_nomad_la_SOURCES += webkitproxy.c webkitproxy.h
libguile_nomad_la_SOURCES += extension.c

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


libguile_nomad_la_LDFLAGS = -module -avoid-version -no-undefined

G_RESOURCES = gresource.xml

EXTRA_DIST = $(G_RESOURCES)

DOT_X_FILES = webkit.x webkitsettings.x webkitproxy.x frame.x app.x util.x minibuffer.x text.x extension.x

BUILT_SOURCES = $(DOT_X_FILES)

CLEANFILES = $(BUILT_SOURCES)

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(AM_CFLAGS) $(libguile_nomad_la_CFLAGS)

SUFFIXES = .x

.c.x:
	@echo "  SNARF" $@;
	@$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

resources.c: $(G_RESOURCES)
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source
