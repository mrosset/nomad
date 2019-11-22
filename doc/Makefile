outs = nomad.html nomad.info nomad nomad.txt # nomad.pdf

all: $(outs)

clean:
	-rm -rf $(outs)

nomad.info: nomad.texi
	makeinfo nomad.texi

nomad: nomad.texi
	makeinfo --html nomad.texi

nomad.html: nomad.texi
	makeinfo nomad.texi --html --no-split

nomad.txt: nomad.texi
	makeinfo nomad.texi --plaintext > nomad.txt

nomad.pdf: nomad.texi
	makeinfo nomad.texi --pdf
