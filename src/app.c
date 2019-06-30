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
nomad_app_init (NomadApp *self)
{
  self->priv = nomad_app_get_instance_private (self);
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
nomad_app_activate (GApplication *self)
{

  NomadAppWindow *win = nomad_app_window_new (NOMAD_APP (self));
  gtk_window_present (GTK_WINDOW (win));
  g_bus_own_name (G_BUS_TYPE_SESSION, BUS_INTERFACE_NAME,
                  G_BUS_NAME_OWNER_FLAGS_NONE,
                  (GBusAcquiredCallback)on_bus_acquired, NULL,
                  (GBusNameLostCallback)on_name_lost, NULL, NULL);
  scm_call_0 (scm_c_public_ref ("nomad app", "app-init"));

  // FIXME: users can start REPL via user-init-hook in $HOME/.nomad. Add
  // documentation for $HOME/.nomad
  scm_c_run_hook (scm_c_public_ref ("nomad init", "user-init-hook"),
                  scm_list_1 (SCM_BOOL_T));
}

static void
nomad_app_class_init (NomadAppClass *class)
{
  G_APPLICATION_CLASS (class)->activate = nomad_app_activate;
}

NomadApp *
nomad_app_new (void)
{
  return g_object_new (NOMAD_APP_TYPE, "application-id", "org.gnu.nomad",
                       "flags", G_APPLICATION_HANDLES_OPEN, NULL);
}

GtkWidget *
nomad_app_get_first_buffer (NomadApp *app)
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkNotebook *nbook = nomad_window_get_notebook (win);

  return gtk_notebook_get_nth_page (nbook, 0);
}

GtkWidget *
nomad_app_get_window (NomadApp *app)
{

  GList *windows;

  windows = gtk_application_get_windows (GTK_APPLICATION (app));

  if (!windows)
    {
      g_critical ("could not find window");
      return NULL;
    }
  return GTK_WIDGET (windows->data);
}

void
nomad_app_next_buffer (NomadApp *app)
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
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
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
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

  win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
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

SCM
nomad_app_get_buffers (NomadApp *app)
{
  SCM list = scm_c_eval_string ("(make-list 0)");
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GList *tabs = nomad_window_get_tabs (win);
  int count = 0;

  for (GList *l = tabs; l != NULL; l = l->next)
    {
      SCM obj = nomad_app_make_buffer (l->data);
      SCM pair = scm_cons (scm_from_int (count), obj);
      list = scm_append (scm_list_2 (list, scm_list_1 (pair)));
      count++;
    }

  return list;
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
  intmax_t status;
  status = g_application_run (G_APPLICATION (app), 0, NULL);
  return scm_from_intmax (status);
}

SCM_DEFINE (scm_nomad_kill, "kill-nomad", 0, 0, 0, (), "Exits Nomad.")
{
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

SCM_DEFINE (scm_nomad_buffer_list, "buffer-alist", 0, 0, 0, (),
            "Return an alist of existing buffers.")
{
  return nomad_app_get_buffers (app);
}

void
run_hints_cb (GObject *source_object, GAsyncResult *res, gpointer user_data)
{
  WebKitWebView *view = user_data;
  GError *error = NULL;
  webkit_web_view_run_javascript_from_gresource_finish (view, res, &error);
  if (error != NULL)
    {
      g_printerr ("Error invoking Javascript resource: %s\n", error->message);
      g_error_free (error);
    }
  g_print ("RESULT CB\n");
}

// FIXME: invoke on main thread?
SCM_DEFINE (scm_nomad_show_hints, "hints", 0, 0, 0, (),
            "Shows WebView html links.")
{

  WebKitWebView *view = nomad_app_get_webview (app);
  GError *error = NULL;

  g_dbus_connection_emit_signal (connection, NULL, BUS_INTERFACE_PATH,
                                 BUS_INTERFACE_NAME, "Changed", NULL, &error);
  if (error != NULL)
    {
      g_printerr ("Error invoking Changed(): %s\n", error->message);
      g_error_free (error);
    }

  webkit_web_view_run_javascript_from_gresource (
      view, "/org/gnu/nomad/hints.js", NULL, run_hints_cb, view);

  return SCM_UNDEFINED;
}

void
nomad_app_register_functions (void *data)
{
#include "app.x"
  scm_c_export ("nomad-version", "start-browser", "restart-nomad",
                "kill-nomad", "buffer-alist", "main-thread", "hints", NULL);
}
