/*
 * wayland.h
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

#ifndef __NOMAD_WAYLAND_H
#define __NOMAD_WAYLAND_H

#include <glib-object.h>

G_BEGIN_DECLS

#define NOMAD_TYPE_WAYLAND_CLIENT nomad_wayland_client_get_type ()

G_DECLARE_FINAL_TYPE (NomadWaylandClient, nomad_wayland_client, NOMAD,
                      WAYLAND_CLIENT, GObject)

/**
 * example:
 *
 * Returns: (transfer full):
 */

void nomad_wayland_server_start ();

void nomad_wayland_client_connect (NomadWaylandClient *self);
void nomad_wayland_client_disconnect (NomadWaylandClient *self);

G_END_DECLS

#endif
