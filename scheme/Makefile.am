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

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES =			\
	nomad/init.scm		\
	nomad/util.scm		\
	nomad/uri.scm		\
	nomad/nomad.scm		\
	nomad/ibuffer.scm	\
	nomad/log.scm	\
	nomad/terminal.scm	\
	nomad/platform.scm	\
	nomad/application.scm	\
	nomad/buffer.scm	\
	nomad/gtk/gi.scm	\
	nomad/gtk/application.scm	\
	nomad/gtk/frame.scm	\
	nomad/gtk/foreign.scm	\
	nomad/gtk/uri.scm	\
	nomad/gtk/buffers.scm	\
	nomad/gtk/widget.scm	\
	nomad/gtk/window.scm	\
	nomad/gtk/util.scm	\
	nomad/graph.scm		\
	nomad/gtk/gtk.scm	\
	nomad/web.scm		\
	nomad/web-mode.scm	\
	nomad/bookmark.scm	\
	nomad/download.scm	\
	nomad/doc.scm		\
	nomad/frame.scm		\
	nomad/html.scm		\
	nomad/options.scm	\
	nomad/repl.scm		\
	nomad/shroud.scm	\
	nomad/text.scm		\
	nomad/views.scm
