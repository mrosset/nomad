# Copyright (C) 2019  Amar M. Singh

# This file is part of Nomad.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

html=nomad.html nomad
outs=nomad.info nomad.txt $(html)
extras=manual nomad.pdf

aux=nomad.aux nomad.tmp nomad.toc

texi_source=nomad.texi

html_args=--css-include=reset.css \
		--css-include=style.css \
		--css-include=manual.css
support_email='nomad-users@nongnu.org'

.ONESHELL:
default: nomad.info nomad.html nomad.txt dir

all: $(outs)

nomad.info: $(texi_source)
	makeinfo $<

dir: nomad.info
	install-info $< $@

nomad: $(texi_source)
	makeinfo $< --html $(html_args)

nomad.html: $(texi_source)
	makeinfo $< --html --no-split $(html_args)

nomad.txt: $(texi_source)
	makeinfo $< --plaintext > nomad.txt

nomad.pdf: $(texi_source)
	makeinfo $< --pdf

manual: $(texi_source)
	./gendocs.sh nomad 'Nomad Reference Manual' --html '$(html_args)' --email '$(support_email)'

.PHONY:
clean:
	-rm -rf $(outs) $(extras) $(aux)

.PHONY:
check:
	@echo Skip doc check

.PHONY:
install:
	@echo Skipped doc install
