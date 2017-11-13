SUBDIRS = src
dist_doc_DATA = README

run: all
	src/wemacs -l test.scm -c "(start-repl) (wemacs-start)"
