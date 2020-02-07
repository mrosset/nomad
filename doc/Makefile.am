## Copyright (C) 2019  Amar M. Singh
## Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>

## This file is part of Nomad.

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

nomad_TEXINFOS = doclicense.texi gpl.texi urls.texi version.texi
info_TEXINFOS  = nomad.texi

AM_MAKEINFOHTMLFLAGS = --css-include=reset.css --css-include=style.css --css-include=manual.css --no-split

EXTRA_DIST = manual.css reset.css style.css gendocs.sh

all-local: info html

install-exec-local: install-html

manual: $(texi_source)
	./gendocs.sh nomad 'Nomad Reference Manual' --html '$(AM_MAKEINFOHTMLFLAGS)' --email '$(support_email)'

api.texi: doc-snarf

info-local: doc-snarf

html-local: doc-snarf

pdf-local: doc-snarf

DOC_SNARF = $(top_builddir)/pre-inst-env $(GUILD) doc-snarf

.PHONY: doc-snarf doc-snarf-api

doc-snarf:
	@$(MAKE) --quiet doc-snarf-api > api.texi

doc-snarf-api:
	@echo "$$DOC_SNARF_HEADER"
	@for i in							\
		$(top_builddir)/scheme/nomad/application.scm		\
		$(top_builddir)/scheme/nomad/bookmark.scm		\
		$(top_builddir)/scheme/nomad/buffer.scm				\
		$(top_builddir)/scheme/nomad/curl.scm			\
		$(top_builddir)/scheme/nomad/doc.scm			\
		$(top_builddir)/scheme/nomad/download.scm		\
		$(top_builddir)/scheme/nomad/frame.scm			\
		$(top_builddir)/scheme/nomad/html.scm			\
		$(top_builddir)/scheme/nomad/init.scm			\
		$(top_builddir)/scheme/nomad/nomad.scm			\
		$(top_builddir)/scheme/nomad/options.scm		\
		$(top_builddir)/scheme/nomad/repl.scm			\
		$(top_builddir)/scheme/nomad/server.scm				\
		$(top_builddir)/scheme/nomad/shroud.scm				\
		$(top_builddir)/scheme/nomad/tests.scm			\
		$(top_builddir)/scheme/nomad/text.scm			\
		$(top_builddir)/scheme/nomad/util.scm			\
		$(top_builddir)/scheme/nomad/views.scm			\
		$(top_builddir)/scheme/nomad/webview.scm		\
		$(top_builddir)/scheme/nomad/widget.scm				\
	; do						\
	  $(GUILD) display-commentary $$i		\
	     | tail +2					\
	     | sed 's,^ ,,';				\
	  $(DOC_SNARF) -l scheme -t $$i;		\
	done
