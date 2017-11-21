SUBDIRS = src ext
dist_doc_DATA = README

run: all
	src/wemacs -L $(HOME)/src/wemacs/scheme -c "(start-repl) (wemacs-start)"
