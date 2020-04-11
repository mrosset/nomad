/*
 * meta-plugin.h
 * Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Nomad
 *
 * Nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Nomad is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __NOMAD_SHELL_PLUGIN_H
#define __NOMAD_SHELL_PLUGIN_H

#include <glib-object.h>
#include <meta/meta-plugin.h>

G_BEGIN_DECLS

#define NOMAD_TYPE_SHELL_PLUGIN nomad_shell_plugin_get_type ()

G_DECLARE_FINAL_TYPE (NomadShellPlugin, nomad_shell_plugin, NOMAD,
                      SHELL_PLUGIN, MetaPlugin)

G_END_DECLS

#endif
