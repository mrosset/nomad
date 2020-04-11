/*
 * wayland.c
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

#include "wayland.h"
#include "plugin.h"
#include <meta/main.h>
#include <wayland-client.h>
#include <wayland-server.h>

// Display

typedef struct _NomadWaylandClientPrivate NomadWaylandClientPrivate;

struct _NomadWaylandClientPrivate
{
  struct wl_display *display;
  struct wl_registry *registry;
};

struct _NomadWaylandClient
{
  GObject parent;
  NomadWaylandClientPrivate *priv;
};

// clang-format off
G_DEFINE_TYPE_WITH_PRIVATE (NomadWaylandClient, nomad_wayland_client, G_TYPE_OBJECT);
// clang-format on

static void
nomad_wayland_client_init (NomadWaylandClient *self)
{
  self->priv = nomad_wayland_client_get_instance_private (self);
}

void
nomad_wayland_start_server ()
{
  const char *socket;

  struct wl_display *display = wl_display_create ();

  if (!display)
    {
      g_critical ("Cound not create Display");
      return;
    }

  socket = wl_display_add_socket_auto (display);
  if (!socket)
    {
      g_critical ("Failed to create socket");
      return;
    }
  setenv ("WAYLAND_DISPLAY", socket, 1);
}

NomadWaylandClient *
nomad_wayland_client_create ()
{
  NomadWaylandClient *display = g_object_new (NOMAD_TYPE_WAYLAND_CLIENT, NULL);
  NomadWaylandClientPrivate *priv
      = nomad_wayland_client_get_instance_private (display);

  priv->display = wl_display_create ();

  if (!display)
    {
      g_critical ("Cound not create Display");
      return NULL;
    }

  return display;
}

static void
nomad_registry_handler (void *data, struct wl_registry *registry, uint32_t id,
                        const char *interface, uint32_t version)
{
  g_debug ("Got a registry event for %s id %d\n", interface, id);
  g_debug ("%s %d\n", interface, id);
}

static void
nomad_registry_remover (void *data, struct wl_registry *registry, uint32_t id)
{
  g_debug ("Got a registry losing event for %d\n", id);
}

void
nomad_wayland_client_connect (NomadWaylandClient *self)
{

  NomadWaylandClientPrivate *priv
      = nomad_wayland_client_get_instance_private (self);

  // clang-format off
  static const struct wl_registry_listener registry_listener
      = { nomad_registry_handler,
          nomad_registry_remover };
  //clang-format on

  priv->display = wl_display_connect (NULL);

  if (!priv->display)
    {
      g_critical ("Could not connect to display");
      return;
    }

  g_debug ("Connnected to Wayland display");

  priv->registry = wl_display_get_registry (priv->display);

  wl_registry_add_listener (priv->registry, &registry_listener, NULL);
  /* wl_display_dispatch (priv->display); */
  wl_display_dispatch_pending (priv->display);
  wl_display_roundtrip (priv->display);
}

void
nomad_wayland_client_disconnect (NomadWaylandClient *self)
{
  NomadWaylandClientPrivate *priv
      = nomad_wayland_client_get_instance_private (self);

  if(!priv->display) {
    g_critical ("Display not connected");
    return;
  }

  wl_display_disconnect (priv->display);

  g_debug ("Disconnected from Wayland display");
}

static void
nomad_wayland_client_class_init (NomadWaylandClientClass *class)
{
}

void
nomad_start_mutter() {
  meta_plugin_manager_set_plugin_type(nomad_shell_plugin_get_type());
  meta_get_option_context();
  /* meta_get_option_context (); */
  meta_init();
  /* meta_get_replace_current_wm(); */
  meta_run();
}
