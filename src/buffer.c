/*
 * buffer.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>
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

#include <webkit2/webkit2.h>

#include "app.h"
#include "buffer.h"
#include "request.h"
#include "window.h"

struct _NomadBufferPrivate
{
  WebKitWebView *view;
  GtkWidget *status;
  GtkWidget *title;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadBuffer, nomad_buffer, GTK_TYPE_BOX)

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  NomadBufferPrivate *priv = user_data;

  gtk_label_set_text (GTK_LABEL (priv->title),
                      webkit_web_view_get_title (view));
  gtk_label_set_text (GTK_LABEL (priv->status),
                      webkit_web_view_get_uri (view));
}

void
nomad_buffer_set_view (NomadBuffer *self, WebKitWebView *view)
{
}

static void
nomad_buffer_init (NomadBuffer *self)
{
  NomadBufferPrivate *priv;

  gtk_widget_init_template (GTK_WIDGET (self));

  self->priv = nomad_buffer_get_instance_private (self);

  priv = self->priv;
  priv->view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  gtk_box_pack_start (GTK_BOX (self), GTK_WIDGET (priv->view), TRUE, TRUE, 0);
  gtk_box_reorder_child (GTK_BOX (self), GTK_WIDGET (priv->view), 0);

  g_signal_connect (priv->view, "load-changed",
                    G_CALLBACK (web_view_load_changed), priv);
}

static void
nomad_buffer_class_init (NomadBufferClass *klass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
                                               "/org/gnu/nomad/buffer.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, status);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, title);
}

GtkWidget *
nomad_buffer_get_title (NomadBuffer *buf)
{
  return buf->priv->title;
}

WebKitWebView *
nomad_buffer_get_view (NomadBuffer *buf)
{
  return buf->priv->view;
}

GtkLabel *
nomad_buffer_get_status (NomadBuffer *buf)
{
  return GTK_LABEL (buf->priv->status);
}

NomadBuffer *
nomad_buffer_new (void)
{
  return g_object_new (NOMAD_TYPE_BUFFER, NULL);
}

void
nomad_buffer_grab_view (NomadBuffer *buf)
{
  gtk_widget_grab_focus (GTK_WIDGET (buf->priv->view));
}

// scheme

void
init_buffer_type (void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol ("buffer");
  finalizer = NULL;
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  buffer_type = scm_make_foreign_object_type (name, slots, finalizer);
}

gboolean
make_buffer_invoke (void *data)
{
  struct request *request = data;
  char *uri = scm_to_locale_string (request->args);
  NomadBuffer *buf = nomad_buffer_new ();
  WebKitWebView *view = nomad_buffer_get_view (buf);
  GtkWidget *win = nomad_app_get_window (NOMAD_APP (app));

  webkit_web_view_load_uri (view, uri);
  nomad_app_window_set_buffer (NOMAD_APP_WINDOW (win), buf);
  nomad_app_add_buffer (NOMAD_APP (app), buf);
  g_free (uri);

  request->response = nomad_app_make_buffer (buf);
  request->done = TRUE;
  return FALSE;
}

SCM_DEFINE (scm_nomad_make_buffer, "make-buffer", 0, 1, 0, (SCM uri),
            "Returns a new foreign object buffer for URI. The buffer will "
            "become the current buffer and load URI.")
{
  struct request *request = &(
      struct request){ .args = uri, .response = SCM_BOOL_F, .done = FALSE };

  g_main_context_invoke (NULL, make_buffer_invoke, request);

  wait_for_response (request);
  return request->response;
}

gboolean
next_buffer_invoke (void *data)
{
  nomad_app_next_buffer (NOMAD_APP (app));
  return FALSE;
}

SCM_DEFINE (scm_nomad_get_next_buffer, "next-buffer", 0, 0, 0, (), "")
{
  g_main_context_invoke (NULL, next_buffer_invoke, NULL);
  return SCM_UNSPECIFIED;
}

gboolean
prev_buffer_invoke (void *data)
{
  nomad_app_prev_buffer (NOMAD_APP (app));
  return FALSE;
}

SCM_DEFINE (scm_nomad_get_prev, "prev-buffer", 0, 0, 0, (), "")
{
  g_main_context_invoke (NULL, prev_buffer_invoke, NULL);
  return SCM_UNSPECIFIED;
}
SCM_DEFINE (scm_nomad_buffer_title, "buffer-title", 1, 0, 0, (SCM buffer),
            "Returns buffer title of BUFFER")
{
  struct buffer *buf = scm_foreign_object_ref (buffer, 0);
  return scm_from_locale_string (webkit_web_view_get_title (buf->view));
}

SCM_DEFINE (scm_nomad_buffer_uri, "buffer-uri", 1, 0, 0, (SCM buffer),
            "Returns buffer title of BUFFER")
{
  struct buffer *buf = scm_foreign_object_ref (buffer, 0);
  return scm_from_locale_string (webkit_web_view_get_uri (buf->view));
}

gboolean
set_buffer_invoke (void *data)
{
  SCM id = data;
  NomadAppWindow *win;
  struct buffer *obj;
  SCM buffer
      = scm_call_1 (scm_c_public_ref ("nomad buffer", "buffer-with-id"), id);

  if (scm_is_bool (buffer))
    {
      g_critical ("buffer not found");
      return FALSE;
    }

  obj = scm_foreign_object_ref (buffer, 0);
  win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  if (obj->buffer == NULL)
    {
      g_critical ("window not found");
      return FALSE;
    }
  nomad_app_window_set_buffer (win, obj->buffer);
  return FALSE;
}

// FIXME: this should return true or false depending on the success of
// trying to switch to buffer.
SCM_DEFINE (scm_nomad_switch_to_buffer, "switch-to-buffer", 1, 0, 0, (SCM id),
            "Switch to buffer with ID. Returns UNSPECIFIED")
{
  g_main_context_invoke (NULL, set_buffer_invoke, id);
  return SCM_UNSPECIFIED;
}

void
nomad_buffer_register_functions (void *data)
{
#include "buffer.x"
  init_buffer_type ();
  scm_c_export ("buffer-title", "buffer-uri", "make-buffer", "current-buffer",
                "next-buffer", "prev-buffer", "scheme-test",
                "switch-to-buffer", NULL);
  return;
}
