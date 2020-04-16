/*
 * compositor.c
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

#ifndef __NOMAD_COMPOSITOR_H
#define __NOMAD_COMPOSITOR_H

#include <glib-object.h>
#include <wayland-client.h>
#include <wayland-server.h>

G_BEGIN_DECLS

struct compositor
{
  struct wl_display *display;
  struct wl_listener listener;
  struct wl_client *client;
  struct wl_global *global;
};

G_END_DECLS

void nomad_compositor_start ();

#endif
