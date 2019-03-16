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

export GUILE_LOAD_PATH=../scheme

AUTOMAKE_OPTIONS = gnu

bin_PROGRAMS = nomad

nomad_SOURCES =			\
	main.cpp		\
	webview.cpp		\
	resource.cpp

AM_CPPFLAGS = -Wall -Werror -Wno-unused-function -fPIC			\
	$(GUILE_CFLAGS)							\
	$(QT_WIDGETS_CFLAGS)						\
	$(QT_WEB_CFLAGS)						\
	$(QT_WEB_WIDGETS_CFLAGS)					\
	-DNOMAD_GUILE_LOAD_COMPILED_PATH=\""$(ccachedir)"\"

nomad_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(VTE_LIBS) $(WEBKIT_LIBS)	\
	$(GTK_SOURCEVIEW_LIBS)						\
	$(QT_WIDGETS_LIBS)						\
	$(QT_WEB_WIDGETS_LIBS)						\
	$(QT_WEB_LIBS)

DOT_X_FILES = webview.x

BUILT_SOURCES = $(DOT_X_FILES)

test: all
	./nomad

format:
	clang-format -i *.c *.h

resource.cpp: $(wildcard *.qml) resources.qrc
	@echo "  RCC" $@;
	@rcc resources.qrc -o $@

SUFFIXES = .x

.cpp.x:
	@echo "  SNARF" $@;
	@CC=gcc $(GUILE_SNARF) -o $@ $< $(AM_CPPFLAGS)

moc_%.cpp: %.h
	@echo "  MOC" $@;
	@moc $(DEFINES) $(INCPATH) $< -o $@
