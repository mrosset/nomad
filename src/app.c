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
#include "window.h"

typedef struct _NomadAppPrivate NomadAppPrivate;

struct _NomadAppPrivate
{
  GList *buffers;
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

NomadApp *app;

static void
nomad_app_activate (GApplication *self)
{
  NomadAppWindow *win;
  NomadBuffer *buf;
  char *c_home_page;
  SCM home_page;

  scm_dynwind_begin (0);
  home_page = scm_c_public_ref ("nomad browser", "default-home-page");
  c_home_page = scm_to_locale_string (home_page);

  win = nomad_app_window_new (NOMAD_APP (self));
  buf = nomad_buffer_new ();
  webkit_web_view_load_uri (nomad_buffer_get_view (buf), c_home_page);
  nomad_app_window_set_buffer (win, buf);
  nomad_app_add_buffer (NOMAD_APP (self), buf);
  scm_dynwind_free (c_home_page);
  scm_dynwind_end ();
  gtk_window_present (GTK_WINDOW (win));
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
nomad_app_switch_to_buffer (NomadApp *app, const char *uri)
{

  /*
   * NomadAppWindow *win;
   * GtkWidget *box;
   * WebKitWebView *view;
   * NomadAppPrivate *priv = nomad_app_get_instance_private (app);
   *
   * win = nomad_app_get_window (app);
   * box = nomad_app_window_get_box (win);
   * view = g_list_first (priv->buffers)->data;
   * webkit_web_view_load_uri (view, uri);
   * gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (view), TRUE, TRUE, 0);
   * nomad_app_window_set_webview (win, view);
   * gtk_widget_show_all (box);
   */
}

void
nomad_app_next_buffer (NomadApp *app)
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GList *l = app->priv->buffers->next;

  if (l == NULL)
    {
      l = g_list_first (app->priv->buffers);
    }

  nomad_app_window_set_buffer (win, NOMAD_BUFFER (l->data));
  app->priv->buffers = l;
}

void
nomad_app_prev_buffer (NomadApp *app)
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GList *l = app->priv->buffers->prev;

  if (l == NULL)
    {
      l = g_list_last (app->priv->buffers);
    }
  nomad_app_window_set_buffer (win, NOMAD_BUFFER (l->data));
  app->priv->buffers = l;
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
nomad_app_add_buffer (NomadApp *app, NomadBuffer *buf)
{
  app->priv->buffers = g_list_append (app->priv->buffers, buf);
}

void
nomad_app_set_current (NomadApp *app, SCM cur)
{
  app->priv->current = cur;
}

SCM
nomad_app_get_current (NomadApp *app)
{
  return app->priv->current;
}

SCM
nomad_app_get_buffer_list (NomadApp *app)
{
  SCM list = scm_c_eval_string ("(make-list 0)");
  int count = 0;

  if (app == NULL)
    {
      g_critical ("APP IS NULL\n");
      return SCM_UNSPECIFIED;
    }

  for (GList *l = app->priv->buffers; l != NULL; l = l->next)
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
  app = nomad_app_new ();
  status = g_application_run (G_APPLICATION (app), 0, NULL);
  return scm_from_intmax (status);
}

SCM_DEFINE (scm_nomad_kill, "kill-nomad", 0, 0, 0, (), "Exits Nomad.")
{
  g_application_quit (G_APPLICATION (app));
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_buffer_list, "buffer-alist", 0, 0, 0, (),
            "Return an alist of existing buffers. The alist is created when "
            "this procedure is called.")
{
  return nomad_app_get_buffer_list (app);
}

void
nomad_app_register_functions (void *data)
{
  app = NOMAD_APP (data);
#include "app.x"
  scm_c_export ("nomad-version", "start-browser", "kill-nomad", "buffer-alist",
                NULL);
}
