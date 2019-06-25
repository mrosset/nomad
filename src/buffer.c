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
  NomadAppWindow *window;
  GtkWidget *title;
  GtkWidget *progress;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadBuffer, nomad_buffer, GTK_TYPE_BOX)

SCM scm_nomad_make_buffer ();

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  NomadBufferPrivate *priv = user_data;
  int fraction = webkit_web_view_get_estimated_load_progress (priv->view);

  gtk_label_set_text (GTK_LABEL (priv->title),
                      webkit_web_view_get_title (view));
  gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (priv->progress), fraction);

  if (fraction == 1)
    {
      gtk_widget_hide (priv->progress);
    }
  else
    {
      gtk_widget_show (priv->progress);
    }
}

gboolean
webview_key_press_cb (GtkWidget *widget, GdkEventKey *event)
{
  GdkModifierType modifiers;
  SCM scm_hook;
  const gchar *key_name;

  scm_hook = scm_c_public_ref ("nomad keymap", "key-press-hook");
  key_name = gdk_keyval_name (event->keyval);
  modifiers = gtk_accelerator_get_default_mod_mask ();

  if ((event->state & modifiers) == GDK_CONTROL_MASK)
    {

      scm_hook = scm_run_hook (
          scm_hook,
          scm_list_3 (scm_variable_ref (scm_c_lookup ("webview-mode-map")),
                      scm_from_int (event->state),
                      scm_from_locale_string (key_name)));
      return TRUE;
    }
  return FALSE;
}

gboolean
decide_policy_cb (WebKitWebView *view, WebKitPolicyDecision *decision,
                  WebKitPolicyDecisionType dtype)
{
  switch (dtype)
    {
    case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
      {
        WebKitNavigationAction *nav
            = webkit_navigation_policy_decision_get_navigation_action (
                WEBKIT_NAVIGATION_POLICY_DECISION (decision));
        WebKitURIRequest *req = webkit_navigation_action_get_request (nav);
        SCM url = scm_from_locale_string (webkit_uri_request_get_uri (req));
        scm_nomad_make_buffer (url);
        return TRUE;
      }
    default:
      return FALSE;
    }

  return TRUE;
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
  g_signal_connect (priv->view, "decide-policy", G_CALLBACK (decide_policy_cb),
                    NULL);
  g_signal_connect (priv->view, "key-press-event",
                    G_CALLBACK (webview_key_press_cb), priv);
}

static void
nomad_buffer_class_init (NomadBufferClass *klass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
                                               "/org/gnu/nomad/buffer.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, title);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, progress);
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
  scm_t_struct_finalize finalizer = NULL;

  name = scm_from_utf8_symbol ("buffer");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  buffer_type = scm_make_foreign_object_type (name, slots, finalizer);
}

gboolean
destroy_buffer_invoke (void *data)
{
  GtkWidget *widget = GTK_WIDGET (data);
  gtk_widget_destroy (widget);
  return FALSE;
}

static void
destroy_buffer (void *data)
{
  g_main_context_invoke (NULL, destroy_buffer_invoke, data);
}

SCM_DEFINE (scm_nomad_make_buffer, "make-web-buffer", 1, 0, 0, (SCM url),
            "Returns a new scheme nomad buffer pointer for URI.")
{
  NomadBuffer *buf = nomad_buffer_new ();
  WebKitWebView *view = nomad_buffer_get_view (buf);
  const char *uri = scm_to_locale_string (url);

  webkit_web_view_load_uri (view, uri);
  return scm_from_pointer (buf, destroy_buffer);
}

gboolean
next_buffer_invoke (void *data)
{
  nomad_app_next_buffer (NOMAD_APP (app));
  return FALSE;
}

gboolean
prev_buffer_invoke (void *data)
{
  struct request *request = data;
  nomad_app_prev_buffer (NOMAD_APP (app));
  request->done = TRUE;
  return FALSE;
}

SCM_DEFINE (scm_nomad_buffer_title, "buffer-title", 1, 0, 0, (SCM buffer),
            "Returns buffer title of BUFFER")
{
  struct buffer *buf = scm_foreign_object_ref (buffer, 0);
  return scm_from_locale_string (webkit_web_view_get_title (buf->view));
}

SCM_DEFINE (scm_nomad_buffer_uri, "primitive-buffer-uri", 1, 0, 0,
            (SCM pointer), "Returns buffer URI of BUFFER pointer")
{
  NomadBuffer *buf;
  buf = NOMAD_BUFFER (scm_to_pointer (pointer));
  return scm_from_locale_string (webkit_web_view_get_uri (buf->priv->view));
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
  nomad_app_window_add_buffer (win, obj->buffer);
  return FALSE;
}

void
nomad_buffer_register_functions (void *data)
{
#include "buffer.x"
  init_buffer_type ();
  scm_c_export ("buffer-title", "primitive-buffer-uri", "make-web-buffer",
                NULL);
  return;
}
