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

#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>

#include "../config.h"
#include "app.h"
#include "buffer.h"
#include "request.h"
#include "window.h"

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

  NomadAppWindow *win;
  GList *windows = gtk_application_get_windows (GTK_APPLICATION (self));

  // We can only have one window
  if (g_list_length (windows) > 0)
    {
      g_warning ("application already started\n");
      return;
    }

  win = nomad_app_window_new (NOMAD_APP (self));

  g_bus_own_name (G_BUS_TYPE_SESSION, BUS_INTERFACE_NAME,
                  G_BUS_NAME_OWNER_FLAGS_NONE,
                  (GBusAcquiredCallback)on_bus_acquired, NULL,
                  (GBusNameLostCallback)on_name_lost, NULL, NULL);

  gtk_window_present (GTK_WINDOW (win));
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
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *nbook = nomad_window_get_notebook (win);

  return gtk_notebook_get_nth_page (nbook, 0);
}

GtkWindow *
nomad_app_get_window ()
{
  NomadApp *app = nomad_app_get_default ();
  return gtk_application_get_active_window (GTK_APPLICATION (app));
}

void
nomad_app_next_buffer (NomadApp *app)
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *nbook = nomad_window_get_notebook (win);

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
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *nbook = nomad_window_get_notebook (win);

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

WebKitWebView *
nomad_app_get_webview (NomadApp *app)
{
  NomadAppWindow *win;

  win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  return nomad_app_window_get_webview (win);
}

SCM
nomad_app_make_buffer (NomadBuffer *buf)
{
  struct buffer *fo_buf
      = (struct buffer *)scm_gc_malloc (sizeof (struct buffer), "buffer");

  fo_buf->view = nomad_buffer_get_view (buf);
  fo_buf->buffer = buf;

  return scm_make_foreign_object_1 (buffer_type, fo_buf);
}

void
nomad_app_remove_buffer (NomadApp *app, NomadBuffer *buf)
{
}

// scheme
SCM_DEFINE (scm_nomad_version, "nomad-version", 0, 0, 0, (),
            "Return string describing the version of Nomad that is running")
{
  return scm_from_utf8_string (VERSION);
}

SCM_DEFINE (scm_nomad_start, "start-browser", 0, 0, 0, (),
            "Start a G_APPLIACTION instance.")
{
  NomadApp *app = nomad_app_new ();
  intmax_t status;
  status = g_application_run (G_APPLICATION (app), 0, NULL);
  return scm_from_intmax (status);
}

SCM_DEFINE (scm_nomad_kill, "kill-nomad", 0, 0, 0, (), "Exits Nomad.")
{
  NomadApp *app = nomad_app_get_default ();
  g_application_quit (G_APPLICATION (app));
  return SCM_UNDEFINED;
}

gboolean
get_main_thread_invoke (void *data)
{
  struct request *request = data;
  request->response = scm_c_eval_string ("(current-thread)");
  request->done = TRUE;
  return FALSE;
}

SCM_DEFINE (scm_nomad_get_main_thread, "main-thread", 0, 0, 0, (),
            "Return the main GApplication thread")
{
  struct request *request
      = &(struct request){ .response = SCM_BOOL_F, .done = FALSE };

  g_main_context_invoke (NULL, get_main_thread_invoke, request);

  wait_for_response (request);
  return request->response;
}

// FIXME: invoke on main thread?
SCM_DEFINE (scm_nomad_dbus_test, "dbus-test", 0, 0, 0, (),
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

SCM_DEFINE (scm_nomad_application_id, "application-id", 0, 0, 0, (),
            "Return string id of nomad application instance")
{
  NomadApp *app = nomad_app_get_default ();
  return scm_from_locale_string (
      g_application_get_application_id (G_APPLICATION (app)));
}

void
nomad_app_register_functions (void *data)
{
#include "app.x"
  scm_c_export ("nomad-version", "start-browser", "restart-nomad",
                "kill-nomad", "buffer-alist", "main-thread", "dbus-test",
                "application-id", NULL);
}
