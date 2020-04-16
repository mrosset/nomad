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

AM_CFLAGS = -Wall -Werror -Wdeclaration-after-statement -DG_LOG_DOMAIN=\"Nomad\"

bin_PROGRAMS = nomad-shell

nomad_shell_SOURCES = main.c
nomad_shell_LDFLAGS = -lguile-nomad -L"@abs_top_builddir@/typelib/.libs"

nomad_shell_CFLAGS   = $(AM_CFLAGS)
nomad_shell_CFLAGS  += $(GUILE_CFLAGS)
nomad_shell_CFLAGS  += $(GLIB_CFLAGS)
nomad_shell_CFLAGS  += $(MUTTER_CFLAGS)
nomad_shell_CFLAGS  += $(CLUTTER_CFLAGS)

nomad_shell_LDADD    = $(GUILE_LIBS)
nomad_shell_LDADD   += $(GLIB_LIBS)
nomad_shell_LDADD   += $(MUTTER_LIBS)
nomad_shell_LDADD   += $(CLUTTER_LIBS)

bin_SCRIPTS = nomad

EXTRA_DIST = nomad.in

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g'	\
		-e 's,[@]LIBDIR[@],$(libdir),g' \
		-e 's,[@]CCACHEDIR[@],$(ccachedir),g'

nomad: nomad.in Makefile
	$(do_subst) < $(srcdir)/nomad.in > $@
	chmod +x nomad

devel_flags = --gapplication-app-id "org.gnu.devel.nomad"

# $(top_builddir)/pre-inst-env ./nomad $(devel_flags)

# ../typelib/main
# $(top_builddir)/pre-inst-env ./nomad $(devel_flags)
# $(top_builddir)/pre-inst-env ./nomad-shell
# $(top_builddir)/pre-inst-env weston --shell $(abs_top_builddir)/typelib/.libs/libnomad-shell.so

test: all
	 ../typelib/main

test-url: all
	echo "test url"
	$(top_builddir)/pre-inst-env ./nomad  "https://www.gnu.org/software/guile" "https://gnu.org" $(devel_flags)
