outs = nomad.html nomad.info nomad nomad.txt # manual # nomad.pdf

all: $(outs)

clean:
	-rm -rf $(outs)

nomad.info: nomad.texi
	makeinfo nomad.texi

nomad: nomad.texi
	makeinfo nomad.texi --html --css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css

nomad.html: nomad.texi
	makeinfo nomad.texi --html --no-split --css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css

nomad.txt: nomad.texi
	makeinfo nomad.texi --plaintext > nomad.txt

nomad.pdf: nomad.texi
	makeinfo nomad.texi --pdf

manual: nomad.texi
	./gendocs.sh nomad 'Nomad Reference Manual' --html '--css-include=reset.css --css-include=style.css --css-include=manual.css' --email 'nomad-users@nongnu.org'
