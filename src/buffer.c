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
#include "util.h"
#include "window.h"

struct _NomadBufferPrivate
{
  WebKitWebView *view;
  NomadAppWindow *window;
  GtkWidget *title;
  SCM buffer;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadBuffer, nomad_buffer, GTK_TYPE_BOX)

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  NomadBuffer *self = NOMAD_BUFFER (user_data);
  NomadBufferPrivate *priv = self->priv;
  gtk_label_set_text (GTK_LABEL (priv->title),
                      webkit_web_view_get_title (priv->view));

  scm_call_2 (scm_c_public_ref ("nomad buffer", "webview-onload"),
              priv->buffer,
              scm_from_locale_string (webkit_web_view_get_uri (priv->view)));
}

static gboolean
web_view_download_request_cb (WebKitWebView *webView, WebKitDownload *download,
                              gboolean *handled)
{
  return TRUE;
}

static gboolean
decide_policy_cb (WebKitWebView *view, WebKitPolicyDecision *decision,
                  WebKitPolicyDecisionType dtype)
{
  switch (dtype)
    {
    case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
      {
        /* WebKitNavigationAction *nav
         *     = webkit_navigation_policy_decision_get_navigation_action (
         *         WEBKIT_NAVIGATION_POLICY_DECISION (decision)); */
        /* WebKitURIRequest *req = webkit_navigation_action_get_request (nav);
         */
        /* SCM url = scm_from_locale_string (webkit_uri_request_get_uri (req));
         */
        /* scm_nomad_make_buffer (url); */
        return TRUE;
      }
    case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
      {
        WebKitResponsePolicyDecision *policy
            = WEBKIT_RESPONSE_POLICY_DECISION (decision);
        WebKitURIResponse *response
            = webkit_response_policy_decision_get_response (policy);
        if (!webkit_response_policy_decision_is_mime_type_supported (policy))
          {
            const gchar *uri = webkit_uri_response_get_uri (response);
            scm_call_1 (scm_c_public_ref ("nomad download", "download"),
                        scm_from_locale_string (uri));
          }
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
  priv->buffer = scm_c_current_buffer ();
  priv->view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  gtk_box_pack_start (GTK_BOX (self), GTK_WIDGET (priv->view), TRUE, TRUE, 0);
  gtk_box_reorder_child (GTK_BOX (self), GTK_WIDGET (priv->view), 0);

  // signals
  g_signal_connect (priv->view, "load-changed",
                    G_CALLBACK (web_view_load_changed), self);
  g_signal_connect (priv->view, "decide-policy", G_CALLBACK (decide_policy_cb),
                    NULL);
}

static void
nomad_buffer_class_init (NomadBufferClass *klass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
                                               "/org/gnu/nomad/buffer.ui");
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
idle_destroy (gpointer data)
{
  gtk_widget_destroy (data);
  return FALSE;
}

SCM_DEFINE (scm_nomad_destroy_web_pointer, "destroy-pointer", 1, 0, 0,
            (SCM pointer), "Destroys widget POINTER")
{
  GtkWidget *widget = scm_to_pointer (pointer);

  if (!widget || !GTK_IS_WIDGET (widget))
    {
      g_warning ("Can't destroy %p\n", widget);
      return SCM_UNSPECIFIED;
    }
  gtk_widget_destroy (widget);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_make_pointer, "make-web-pointer", 0, 0, 0, (),
            "Returns a new scheme nomad buffer control")
{
  NomadBuffer *buf = nomad_buffer_new ();
  return scm_from_pointer (buf, NULL);
}

GtkWidget *
tab_label_new (int id)
{
  SCM label = scm_number_to_string (scm_from_int (id), scm_from_int (10));
  return gtk_label_new (scm_to_locale_string (label));
}

SCM_DEFINE (scm_switch_to_pointer_x, "switch-to-pointer", 1, 0, 0,
            (SCM pointer), "Sets the current tab to the given POINTER")
{
  gint page;
  GtkWidget *view;
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *notebook = nomad_window_get_notebook (win);

  if (SCM_POINTER_P (pointer))
    {
      view = scm_to_pointer (pointer);
      page = gtk_notebook_page_num (notebook, view);
      if (page < 0)
        {
          page = gtk_notebook_append_page (
              notebook, view,
              tab_label_new (gtk_notebook_get_n_pages (notebook)));
        }
      gtk_widget_show_all (view);
      gtk_notebook_set_current_page (notebook, page);
      if (page != gtk_notebook_get_current_page (notebook))
        {
          g_warning ("Paged not switched to %d", page);
        }
    }
  else
    g_warning ("warning: not given a pointer\n");
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_buffer_title, "buffer-title", 1, 0, 0, (SCM buffer),
            "Returns buffer title of BUFFER")
{
  struct buffer *buf = scm_foreign_object_ref (buffer, 0);
  return scm_from_locale_string (webkit_web_view_get_title (buf->view));
}

SCM_DEFINE (scm_nomad_set_pointer_uri_x, "set-pointer-uri", 2, 0, 0,
            (SCM pointer, SCM uri),
            "Sets POINTER URI. This causes the webview to load URI")
{
  NomadBuffer *buf;
  char *c_uri;

  scm_dynwind_begin (0);

  c_uri = scm_to_locale_string (uri);
  scm_dynwind_unwind_handler (free, c_uri, SCM_F_WIND_EXPLICITLY);

  buf = scm_to_pointer (pointer);
  webkit_web_view_load_uri (buf->priv->view, c_uri);
  scm_dynwind_end ();

  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_pointer_load_content, "set-pointer-content", 3, 0, 0,
            (SCM pointer, SCM content, SCM base_uri),
            "Load CONTENT/HTML in BUFFER. (P.S. can also be used to "
            "load any other type of text.")
{
  gchar *c_text;
  gchar *c_uri;
  NomadBuffer *buf;
  scm_dynwind_begin (0);

  buf = scm_to_pointer (pointer);
  c_text = scm_to_locale_string (content);
  c_uri = scm_to_locale_string (base_uri);
  scm_dynwind_unwind_handler (free, c_text, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (free, c_uri, SCM_F_WIND_EXPLICITLY);

  if (buf->priv->view == NULL)
    {
      scm_dynwind_end ();
      return SCM_BOOL_F;
    }

  webkit_web_view_load_html (buf->priv->view, c_text, c_uri);
  scm_dynwind_end ();

  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_buffer_uri, "pointer-uri", 1, 0, 0, (SCM pointer),
            "Returns buffer URI of POINTER")
{
  NomadBuffer *buf = NOMAD_BUFFER (scm_to_pointer (pointer));
  const char *c_uri = webkit_web_view_get_uri (buf->priv->view);

  if (c_uri)
    {
      return scm_from_locale_string (c_uri);
    }
  return scm_from_utf8_string ("nomad://");
}

SCM_DEFINE (scm_nomad_number_tabs, "number-tabs", 0, 0, 0, (),
            "Return the total number of tabs")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *notebook = GTK_NOTEBOOK (nomad_window_get_notebook (win));
  return scm_from_int (gtk_notebook_get_n_pages (notebook));
}

SCM_DEFINE (scm_nomad_notebook_contains, "notebook-contains", 1, 0, 0,
            (SCM buffer), "Return #t if notebook contains BUFFER")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *notebook = nomad_window_get_notebook (win);
  SCM pointer = scm_call_1 (
      scm_c_public_ref ("nomad webview", "buffer-pointer"), buffer);
  GtkWidget *widget = scm_to_pointer (pointer);
  gint page = gtk_notebook_page_num (notebook, widget);

  if (page >= 0)
    {
      return SCM_BOOL_T;
    }

  return SCM_BOOL_F;
}

SCM_DEFINE (scm_nomad_notebook_insert, "notebook-insert", 2, 0, 0,
            (SCM buffer, SCM INDEX), "Inserts BUFFER into notebook at INDEX")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *notebook = nomad_window_get_notebook (win);
  SCM pointer = scm_call_1 (
      scm_c_public_ref ("nomad webview", "buffer-pointer"), buffer);
  GtkWidget *widget = scm_to_pointer (pointer);
  gint page
      = gtk_notebook_insert_page (notebook, widget, tab_label_new (0), 0);

  if (page >= 0)
    {
      gtk_widget_show_all (widget);
      return SCM_BOOL_T;
    }

  return SCM_BOOL_F;
}

void
nomad_buffer_register_functions (void *data)
{
#include "buffer.x"
  init_buffer_type ();
  scm_c_export ("buffer-title", "pointer-uri", "set-pointer-uri",
                "make-web-pointer", "switch-to-pointer", "destroy-pointer",
                "set-pointer-content", "number-tabs", "notebook-contains",
                "notebook-insert", NULL);
  return;
}
