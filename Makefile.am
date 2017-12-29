SUBDIRS = src ext scheme

ACLOCAL_AMFLAGS = -I m4

dist_doc_DATA = README

export GUILE_LOAD_PATH=$(PWD)/scheme

run: all
	src/nomad
