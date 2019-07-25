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

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
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

gboolean
idle_destroy (gpointer data)
{
  gtk_widget_destroy (data);
  return FALSE;
}

// scheme
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
  scm_c_export ("buffer-title", "pointer-uri", "set-pointer-uri",
                "make-web-pointer", "switch-to-pointer", "destroy-pointer",
                "set-pointer-content", "number-tabs", "notebook-contains",
                "notebook-insert", NULL);
  return;
}
