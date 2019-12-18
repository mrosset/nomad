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
nomad_TEXINFOS=doclicense.texi gpl.texi urls.texi version.texi
info_TEXINFOS=nomad.texi

AM_MAKEINFOHTMLFLAGS=--css-include=reset.css --css-include=style.css --css-include=manual.css --no-split

all-local:
	make info
	make html

install-exec-local:
	make install-html

manual: $(texi_source)
	./gendocs.sh nomad 'Nomad Reference Manual' --html '$(AM_MAKEINFOHTMLFLAGS)' --email '$(support_email)'
