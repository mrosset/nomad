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
#include "scheme.h"
#include <assert.h>
#include <libguile.h>
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

struct wl_compositor *compositor = NULL;
struct wl_surface *surface;
struct wl_shell *shell;
struct wl_shell_surface *shell_surface;

static void
global_registry_handler (void *data, struct wl_registry *registry, uint32_t id,
                         const char *interface, uint32_t version)
{

  if (strcmp (interface, "wl_compositor") == 0)
    {
      g_debug ("binding interface %s", interface);
      compositor
          = wl_registry_bind (registry, id, &wl_compositor_interface, 1);
    }
  else if (strcmp (interface, "wl_shell") == 0)
    {
      g_debug ("binding interface %s", interface);
      shell = wl_registry_bind (registry, id, &wl_shell_interface, version);
    }
}

static void
global_registry_remover (void *data, struct wl_registry *registry, uint32_t id)
{
  printf ("Got a registry losing event for %d\n", id);
}

void
nomad_wayland_client_connect (NomadWaylandClient *self)
{

  NomadWaylandClientPrivate *priv
      = nomad_wayland_client_get_instance_private (self);

  static const struct wl_registry_listener registry_listener
      = { global_registry_handler, global_registry_remover };

  priv->display = wl_display_connect (NULL);

  assert (priv->display && "Could not connect to display");

  priv->registry = wl_display_get_registry (priv->display);

  assert (priv->registry);
  wl_registry_add_listener (priv->registry, &registry_listener, NULL);

  wl_display_dispatch_pending (priv->display);
  wl_display_roundtrip (priv->display);

  g_debug ("Connnected to Wayland display");

  if (compositor == NULL)
    {
      g_error ("Can't find compositor");
      exit (0);
    }
  else
    {
      g_debug ("Found compositor\n");
    }

  surface = wl_compositor_create_surface (compositor);
  if (surface == NULL)
    {
      g_error ("Can't create surface\n");
      exit (1);
    }
  else
    {
      g_debug ("Created surface\n");
    }

  /* shell_surface = wl_shell_get_shell_surface (shell, surface);
   * if (shell_surface == NULL)
   *   {
   *     g_error ("Can't create shell surface\n");
   *   }
   * else
   *   {
   *     g_debug ("Created shell surface\n");
   *   } */
  /* wl_shell_surface_set_toplevel (shell_surface); */
}

void
nomad_wayland_client_disconnect (NomadWaylandClient *self)
{
  NomadWaylandClientPrivate *priv
      = nomad_wayland_client_get_instance_private (self);

  if (!priv->display)
    {
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

struct compositor
{
  struct wl_display *display;
  struct wl_listener listener;
  struct wl_client *client;
  struct wl_global *global;
};

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

gpointer
display_run (gpointer data)
{
  struct compositor *compositor = data;

  wl_display_run (compositor->display);

  wl_display_destroy_clients (compositor->display);
  wl_display_destroy (compositor->display);
  return NULL;
}

void
setup_compositor (struct compositor *compositor)
{
  const char *socket;

  compositor->display = wl_display_create ();
  compositor->global
      = wl_global_create (compositor->display, &wl_compositor_interface, 4,
                          NULL, &bind_compositor);

  socket = wl_display_add_socket_auto (compositor->display);

  g_debug ("Server Started");

  setenv ("WAYLAND_DISPLAY", socket, 1);

  if (fork () == 0)
    {
      execl ("/bin/sh", "/bin/sh", "-c", "weston-info", (void *)NULL);
    }

  g_thread_new ("compositor", display_run, compositor);
  sleep (1);
}

void
nomad_wayland_server_start ()
{

  struct compositor compositor = { 0 };

  setup_compositor (&compositor);
}
