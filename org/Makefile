outs = README.html README.txt

all: $(outs)

clean:
	-rm $(outs)

README.texi: README.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-texinfo-export-to-texinfo))"

README.html: README.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-html-export-to-html))"

README.txt: README.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-ascii-export-to-ascii))"
