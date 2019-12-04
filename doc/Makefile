outs = nomad.html nomad.info nomad nomad.txt # nomad.pdf

all: $(outs)

clean:
	-rm -rf $(outs)

nomad.info: nomad.texi
	makeinfo nomad.texi

nomad: nomad.texi
	makeinfo --html --css-ref=https://www.gnu.org/software/gnulib/manual.css nomad.texi

nomad.html: nomad.texi
	makeinfo nomad.texi --html --no-split --css-ref=https://www.gnu.org/software/gnulib/manual.css

nomad.txt: nomad.texi
	makeinfo nomad.texi --plaintext > nomad.txt

nomad.pdf: nomad.texi
	makeinfo nomad.texi --pdf
