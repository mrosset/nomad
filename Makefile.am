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

SUBDIRS = guile scheme src

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = init.el
## FIXME this is hackish. promote init.el to emacs package
nomaddir = "$(datadir)/nomad"
nomad_DATA = init.el

dist_doc_DATA = README

clean-scheme:
	$(MAKE) -C ./scheme clean

test: check
	$(MAKE) -C ./src test

test-install: install
	nomad

guix-local:
	guix build -f ./guix-local.scm

install-guix-local:
	guix package -f ./guix-local.scm

format:
	$(MAKE) -C guile format

rebuild: clean all

CLEANFILES =		\
	check-guile.log \
	check-guile.trs \
	test-suite.log

DISTCLEANFILES = check-guile.log

attach:
	$(MAKE) -C docker attach
