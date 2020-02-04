# Copyright (C) 2020 Amar M. Singh

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

iconsdir = $(datadir)/icons/hicolor

nobase_dist_icons_DATA = \
    16x16/apps/nomad.png \
    22x22/apps/nomad.png \
    24x24/apps/nomad.png \
    32x32/apps/nomad.png \
    48x48/apps/nomad.png

gtk_update_icon_cache = gtk-update-icon-cache -f -t $(iconsdir)

install-data-hook: update-icon-cache

uninstall-hook: update-icon-cache

update-icon-cache:
    @-if test -z "$(DESTDIR)" then \
        echo "Updating GTK icon cache." \
        $(gtk_update_icon_cache) \
    else \
        echo "*** Icon cache not updated. After (un)install, run this:" \
        echo "*** $(gtk_update_icon_cache)" \
    fi
