/*
 * compositor.h
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

#include "compositor.h"
#include <unistd.h>

gpointer
display_run (gpointer data)
{
  struct compositor *compositor = data;

  wl_display_run (compositor->display);

  wl_display_destroy_clients (compositor->display);
  wl_display_destroy (compositor->display);
  return NULL;
}

static void
create_surface (struct wl_client *client, struct wl_resource *resource,
                uint32_t id)
{
  g_debug ("%s Not Implimented", __func__);
}

static void
create_region (struct wl_client *client, struct wl_resource *resource,
               uint32_t id)
{
  g_debug ("%s Not Implimented", __func__);
}

static const struct wl_compositor_interface compositor_impl = {
  .create_surface = create_surface,
  .create_region = create_region,
};

static void
bind_compositor (struct wl_client *client, void *data, uint32_t version,
                 uint32_t id)
{
  struct wl_resource *resource;

  resource
      = wl_resource_create (client, &wl_compositor_interface, version, id);
  if (!resource)
    {
      wl_client_post_no_memory (client);
      return;
    }
  wl_resource_set_implementation (resource, &compositor_impl, NULL, NULL);
}

void
nomad_compositor_start ()
{

  struct compositor compositor = { 0 };
  const char *socket;

  compositor.display = wl_display_create ();
  compositor.global = wl_global_create (
      compositor.display, &wl_compositor_interface, 4, NULL, &bind_compositor);

  socket = wl_display_add_socket_auto (compositor.display);

  g_debug ("Server Started");

  setenv ("WAYLAND_DISPLAY", socket, 1);

  if (fork () == 0)
    {
      execl ("/bin/sh", "/bin/sh", "-c", "weston-info", (void *)NULL);
    }

  g_thread_new ("compositor", display_run, &compositor);
  /* sleep (1); */
}
