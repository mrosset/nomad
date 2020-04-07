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

ACLOCAL_AMFLAG = -I m4

AM_CFLAGS = -Wall -Werror

libnomad_LTLIBRARIES = libnomad.la
libnomaddir          = $(libexecdir)/nomad/
libnomad_la_SOURCES  = ext.c resources.c
libnomad_la_CFLAGS   = $(AM_CFLAGS) $(WEB_EXTENSION_CFLAGS) $(GUILE_CFLAGS)
libnomad_la_LIBADD   = $(WEB_EXTENSION_LIBS) $(GUILE_LIBS) $(GLIB_LIBS)
libnomad_la_LDFLAGS  = -module -avoid-version -no-undefined

BUILT_SOURCES = resources.c
G_RESOURCES   = gresource.xml
EXTRA_DIST    = $(G_RESOURCES) hints.js
CLEANFILES    = $(BUILT_SOURCES)

resources.c: $(G_RESOURCES)
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source
