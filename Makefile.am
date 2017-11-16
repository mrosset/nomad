SUBDIRS = src ext
dist_doc_DATA = README

run: all install
	src/wemacs -c "(start-repl) (wemacs-start)"
