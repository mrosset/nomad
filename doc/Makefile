outs = nomad.html nomad.txt nomad.texi nomad.info nomad

all: $(outs)

clean:
	-rm $(outs)

nomad.texi: nomad.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-texinfo-export-to-texinfo))"

nomad.info: nomad.texi
	makeinfo nomad.texi

nomad: nomad.texi
	makeinfo nomad.texi --html

nomad.html: nomad.texi
	makeinfo nomad.texi --html --no-split

nomad.txt: nomad.org
	makeinfo nomad.texi --plaintext > nomad.txt

nomad.pdf: nomad.org
	makeinfo nomad.texi --pdf
