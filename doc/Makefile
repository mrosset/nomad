outs = nomad.html nomad.txt nomad.texi nomad.info

all: $(outs)

clean:
	-rm $(outs)

nomad.texi: nomad.org
	emacs -q --batch --eval "(progn (find-file \"$<\") \
				(org-texinfo-export-to-texinfo))"

nomad.info: nomad.texi
	makeinfo nomad.texi

nomad.html: nomad.texi
	makeinfo nomad.texi --html

nomad.txt: nomad.org
	makeinfo nomad.texi --plaintext > nomad.txt

nomad.pdf: nomad.org
	makeinfo nomad.texi --pdf
