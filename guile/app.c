/*
 * app.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Nomad
 *
 * Nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 * Nomad is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <emacsy.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>

#include "../config.h"
#include "app.h"
#include "frame.h"
#include "text.h"
#include "util.h"

#define BUS_INTERFACE_NAME "org.gnu.nomad.webview"
#define BUS_INTERFACE_PATH "/org/gnu/nomad/webview"

// Declarations
static void nomad_app_activate_cb (GApplication *self);

static GDBusConnection *connection;
static GDBusInterfaceInfo *interface;
static guint bus_id;

static const gchar introspection_xml[]
    = "<node>"
      "<interface name='" BUS_INTERFACE_NAME "'>"
      "<signal name='Changed'>"
      "</signal>"
      "</interface>"
      "</node>";

typedef struct _NomadAppPrivate NomadAppPrivate;

struct _NomadAppPrivate
{
  SCM current;
};

struct _NomadApp
{
  GtkApplication parent;
  NomadAppPrivate *priv;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadApp, nomad_app, GTK_TYPE_APPLICATION);

static void
startup (GApplication *app, gpointer data)
{
  WebKitCookieManager *cookie_manager;
  char *c_cookie_file;
  SCM cookie_file = scm_variable_ref (scm_c_lookup ("user-cookie-file"));
  SCM use_cookies = scm_variable_ref (scm_c_lookup ("use-cookies?"));

  // Cookies

  c_cookie_file = scm_to_locale_string (cookie_file);

  if (scm_is_true (use_cookies))
    {
      cookie_manager = webkit_web_context_get_cookie_manager (
          webkit_web_context_get_default ());

      webkit_cookie_manager_set_persistent_storage (
          cookie_manager, c_cookie_file,
          WEBKIT_COOKIE_PERSISTENT_STORAGE_SQLITE);
    }

  g_free (c_cookie_file);
}

static void
shutdown (GApplication *app, gpointer data)
{
  scm_call_0 (scm_c_public_ref ("nomad app", "shutdown"));
}

NomadApp *
nomad_app_get_default ()
{
  return NOMAD_APP (g_application_get_default ());
}

// FIXME: if the application is not running this will get called and fail
static void
nomad_app_open_cb (GApplication *application, GFile **files, gint n_files,
                   gchar *hint, gpointer user_data)
{
  const char *uri = g_file_get_uri (files[0]);
  scm_call_1 (scm_c_public_ref ("nomad buffer", "make-buffer"),
              scm_from_locale_string (uri));
}

static void
nomad_app_init (NomadApp *self)
{
  self->priv = nomad_app_get_instance_private (self);
  g_signal_connect (G_APPLICATION (self), "open",
                    G_CALLBACK (nomad_app_open_cb), NULL);
}

static void
on_bus_acquired (GDBusConnection *con, const gchar *name)
{
  GError *error = NULL;
  GDBusNodeInfo *node
      = g_dbus_node_info_new_for_xml (introspection_xml, &error);

  connection = con;
  if (node == NULL)
    goto fail;
  interface = g_dbus_node_info_lookup_interface (node, BUS_INTERFACE_NAME);
  if (interface == NULL)
    goto fail;

  bus_id = g_dbus_connection_register_object (
      connection, BUS_INTERFACE_PATH, interface, NULL, NULL, NULL, &error);

  if (bus_id == 0)
    goto fail;

  return;

fail:
  if (error != NULL)
    {
      g_critical ("Error hooking up web extension DBus interface: %s",
                  error->message);
      g_clear_error (&error);
    }
  else
    {
      g_critical ("Unknown error hooking up web extension DBus interface");
    }
}

static void
on_name_lost (GDBusConnection *connection, const gchar *name)
{
  if (connection == NULL)
    {
      g_error ("Couldn't connect to DBus for name %s", name);
      return;
    }

  if (!g_dbus_connection_unregister_object (connection, bus_id))
    g_critical ("Trouble unregistering object");
}

static void
nomad_app_activate_cb (GApplication *self)
{

  NomadAppFrame *frame;
  GList *frames = gtk_application_get_windows (GTK_APPLICATION (self));

  // We can only have one frame
  if (g_list_length (frames) > 0)
    {
      g_warning ("application already started\n");
    }

  frame = nomad_app_frame_new (NOMAD_APP (self));

  g_bus_own_name (G_BUS_TYPE_SESSION, BUS_INTERFACE_NAME,
                  G_BUS_NAME_OWNER_FLAGS_NONE,
                  (GBusAcquiredCallback)on_bus_acquired, NULL,
                  (GBusNameLostCallback)on_name_lost, NULL, NULL);

  gtk_window_present (GTK_WINDOW (frame));
  scm_call_0 (scm_c_public_ref ("nomad app", "app-init"));
}

static void
nomad_app_class_init (NomadAppClass *class)
{
  G_APPLICATION_CLASS (class)->activate = nomad_app_activate_cb;
  /* G_APPLICATION_CLASS (class)->open = nomad_app_open_cb; */
}

NomadApp *
nomad_app_new ()
{
  return g_object_new (
      NOMAD_APP_TYPE, "application-id", "org.gnu.nomad", "flags",
      G_APPLICATION_HANDLES_OPEN | G_APPLICATION_CAN_OVERRIDE_APP_ID, NULL);
}

GtkWidget *
nomad_app_get_first_buffer (NomadApp *app)
{
  NomadAppFrame *frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  GtkNotebook *nbook = nomad_app_frame_get_notebook (frame);

  return gtk_notebook_get_nth_page (nbook, 0);
}

GtkWindow *
nomad_app_get_frame ()
{
  NomadApp *app = nomad_app_get_default ();
  return gtk_application_get_active_window (GTK_APPLICATION (app));
}

void
nomad_app_next_buffer (NomadApp *app)
{
  NomadAppFrame *frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  GtkNotebook *nbook = nomad_app_frame_get_notebook (frame);

  // If this is the last tab goto the first tab
  if (gtk_notebook_get_n_pages (nbook) - 1
      == gtk_notebook_get_current_page (nbook))
    {
      gtk_notebook_set_current_page (nbook, 0);
    }
  else
    {

      gtk_notebook_next_page (nbook);
    }
}

void
nomad_app_prev_buffer (NomadApp *app)
{
  NomadAppFrame *frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  GtkNotebook *nbook = nomad_app_frame_get_notebook (frame);

  // If this is the first tab goto the last tab
  if (gtk_notebook_get_current_page (nbook) == 0)
    {
      gtk_notebook_set_current_page (nbook,
                                     gtk_notebook_get_n_pages (nbook) - 1);
    }
  else
    {

      gtk_notebook_prev_page (nbook);
    }
}

// scheme
SCM_DEFINE_PUBLIC (
    scm_nomad_version, "nomad-version", 0, 0, 0, (),
    "Return string describing the version of Nomad that is running")
{
  return scm_from_utf8_string (VERSION);
}

SCM_DEFINE_PUBLIC (scm_nomad_start, "start-nomad", 1, 0, 0, (SCM lst),
                   "Starts nomad application with LST as program arguments")
{
  intmax_t status;
  NomadApp *app = nomad_app_new ();
  int argc = scm_to_int (scm_length (lst));
  SCM ptr = scm_nomad_list_to_argv (lst);

  char **argv = g_strdupv (scm_to_pointer (ptr));

  GOptionEntry entries[]
      = { { "quick", 'Q', G_OPTION_FLAG_NONE, G_OPTION_ARG_NONE, NULL,
            "Start nomad without using user-init-file", NULL },
          { NULL } };

  g_signal_connect (app, "startup", G_CALLBACK (startup), NULL);
  g_signal_connect (app, "shutdown", G_CALLBACK (shutdown), NULL);

  g_application_add_main_option_entries (G_APPLICATION (app), entries);

  scm_call_0 (scm_c_public_ref ("nomad init", "init"));

  status = g_application_run (G_APPLICATION (app), argc, argv);

  return scm_from_intmax (status);
}

void
nomad_app_run (NomadApp *app)
{
  SCM lst = scm_call_0 (scm_c_public_ref ("guile", "command-line"));

  scm_nomad_start (lst);
}

SCM_DEFINE_PUBLIC (scm_nomad_kill, "kill-nomad", 0, 0, 0, (), "Exits Nomad.")
{
  NomadApp *app = nomad_app_get_default ();
  g_application_quit (G_APPLICATION (app));
  return SCM_UNDEFINED;
}

// FIXME: invoke on main thread?
SCM_DEFINE_PUBLIC (scm_nomad_dbus_test, "dbus-test", 0, 0, 0, (),
                   "Shows WebView html links.")
{

  GError *error = NULL;

  g_dbus_connection_emit_signal (connection, NULL, BUS_INTERFACE_PATH,
                                 BUS_INTERFACE_NAME, "Changed", NULL, &error);
  if (error != NULL)
    {
      g_printerr ("Error invoking Changed(): %s\n", error->message);
      g_error_free (error);
    }

  return SCM_UNDEFINED;
}

void
nomad_app_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "app.x"
#endif
}
