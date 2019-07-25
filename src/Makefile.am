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

bin_SCRIPTS = nomad

EXTRA_DIST = nomad.in

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g'

nomad: nomad.in Makefile
	$(do_subst) < $(srcdir)/nomad.in > $@
	chmod +x nomad

.PHONY: format

format:
	 clang-format -i *.c *.h

test: all
	$(top_builddir)/pre-inst-env ./nomad --gapplication-app-id "org.devel.nomad"

test-url: all
	echo "test url"
	$(top_builddir)/pre-inst-env ./nomad "https://bufio.org" --listen $(DEVEL_SOCKET)
