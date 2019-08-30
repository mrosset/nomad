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

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES) $(TESTS) nomad/lib.in
CLEANFILES = $(GOBJECTS) nomad/lib.scm

guile_install_go_files = install-nobase_ccacheDATA
$(guile_install_go_files): install-nobase_modDATA

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g' \
		-e 's,[@]LIBEXEC[@],$(libexecdir),g' \
		-e 's,[@]TOPBUILD[@],$(abs_top_builddir),g'

nomad/lib.scm: nomad/lib.in Makefile
	$(do_subst) < $(srcdir)/$< > $@

SOURCES =			\
	nomad/lib.scm		\
	nomad/app.scm		\
	nomad/bookmark.scm	\
	nomad/buffer.scm	\
	nomad/download.scm	\
	nomad/doc.scm		\
	nomad/eval.scm		\
	nomad/frame.scm		\
	nomad/html.scm		\
	nomad/init.scm		\
	nomad/minibuffer.scm	\
	nomad/options.scm	\
	nomad/widget.scm	\
	nomad/repl.scm		\
	nomad/shroud.scm	\
	nomad/server.scm	\
	nomad/tests.scm		\
	nomad/text.scm		\
	nomad/util.scm		\
	nomad/views.scm		\
	nomad/webview.scm	\
	nomad/webkit.scm	\
	nomad/webkit-settings.scm	\
	nomad/webkit-proxy.scm	\
	nomad/nomad.scm

TESTS =			\
	tests/lib.scm		\
	tests/buffer.scm	\
	tests/download.scm	\
	tests/eval.scm		\
	tests/frame.scm		\
	tests/init.scm		\
	tests/options.scm	\
	tests/text.scm		\
	tests/util.scm		\
	tests/webkit.scm	\
	tests/webview.scm

TEST_EXTENSIONS = .scm

SCM_LOG_DRIVER =					\
	$(top_builddir)/pre-inst-env			\
	$(GUILE) --no-auto-compile -e main		\
	$(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"
