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

GOBJECTS = $(SOURCES:%.scm=%.go)
GUILE_WARNINGS = -Wunbound-variable -Wmacro-use-before-definition -Warity-mismatch -Wformat

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_ccache_DATA = $(GOBJECTS)

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
CLEANFILES = $(GOBJECTS)

guile_install_go_files = install-nobase_ccacheDATA
$(guile_install_go_files): install-nobase_modDATA

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GUILEC) GUILE_AUTO_COMPILE=0			\
	$(GUILD) compile --target="$(host)" $(GUILE_WARNINGS)	\
	  -L "$(abs_srcdir)" -L "$(abs_builddir)"		\
	  -o "$@" "$<"

SOURCES =			\
	nomad/app.scm		\
	nomad/browser.scm	\
	nomad/buffer.scm	\
	nomad/events.scm	\
	nomad/frame.scm		\
	nomad/init.scm		\
	nomad/keymap.scm	\
	nomad/options.scm	\
	nomad/repl.scm		\
	nomad/server.scm	\
	nomad/util.scm		\
	nomad/webview.scm
