/*
 * ext.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

 * This file is part of Nomad

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

#include <dbus/dbus.h>
#include <libguile.h>
#include <webkit2/webkit-web-extension.h>

#define BUS_INTERFACE_NAME "org.gnu.nomad.webview"
#define BUS_INTERFACE_PATH "/org/gnu/nomad/webview"

WebKitWebPage *page = NULL;

static void
web_page_loaded_callback (WebKitWebPage *web_page, gpointer user_data)
{
  g_print("PAGE created\n");
  page = web_page;
}

static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage *web_page, char *user_data)
{
  g_signal_connect (web_page, "document-loaded",
                    G_CALLBACK (web_page_loaded_callback), NULL);
}

void
changed ()
{
  g_print ("Changed Signal\n");
}

void *
on_name_appear (GDBusConnection *connection, const char *name,
                const char *name_owner, void *user_data)
{
  int id = g_dbus_connection_signal_subscribe (
      connection, NULL, BUS_INTERFACE_NAME, "Changed", BUS_INTERFACE_PATH,
      NULL, G_DBUS_SIGNAL_FLAGS_NONE, changed, NULL, NULL);

  g_print ("APPEAR\n");
  if (id == 0)
    {
      g_critical ("Could not connect to signal 'Changed'\n");
    }

  return NULL;
}

void *
on_name_lost (GDBusConnection *connection, const char *name,
              const char *name_owner, gpointer user_data)
{
  g_print ("LOST\n");
  return NULL;
}

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data (WebKitWebExtension *extension,
                                                GString *user_data)
{
  g_signal_connect (extension, "page-created",
                    G_CALLBACK (web_page_created_callback), user_data);

  /*
   * g_signal_connect (webkit_script_world_get_default (),
   *                   "window-object-cleared",
   *                   G_CALLBACK (window_object_cleared_callback), NULL);
   */

  g_bus_watch_name (G_BUS_TYPE_SESSION, BUS_INTERFACE_NAME,
                    G_BUS_NAME_OWNER_FLAGS_NONE,
                    (GBusNameAppearedCallback)on_name_appear,
                    (GBusNameLostCallback)on_name_lost, NULL, NULL);
}
