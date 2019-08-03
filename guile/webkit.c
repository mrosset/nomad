/*
 * webkit.c
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

#include "webkit.h"
#include "app.h"
#include "util.h"
/* #include <glib-object.h> */
#include <libguile.h>
#include <webkit2/webkit2.h>

typedef struct _NomadWebViewPrivate NomadWebViewPrivate;

struct _NomadWebViewPrivate
{
  SCM buffer;
};

struct _NomadWebView
{
  WebKitWebView parent;
  NomadWebViewPrivate *priv;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadWebView, nomad_web_view,
                            WEBKIT_TYPE_WEB_VIEW);

static gboolean
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
        scm_call_1 (scm_c_public_ref ("nomad buffer", "make-buffer"), url);
        return TRUE;
      }
      /*     case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
       *       {
       *         WebKitResponsePolicyDecision *policy
       *             = WEBKIT_RESPONSE_POLICY_DECISION (decision);
       *         WebKitURIResponse *response
       *             = webkit_response_policy_decision_get_response (policy);
       *         if (!webkit_response_policy_decision_is_mime_type_supported
       * (policy))
       *           {
       *             const gchar *uri = webkit_uri_response_get_uri (response);
       *             scm_call_1 (scm_c_public_ref ("nomad download",
       * "download"), scm_from_locale_string (uri));
       *           }
       *         return TRUE;
       *       } */
    default:
      return FALSE;
    }

  return TRUE;
}

static void
load_changed_cb (NomadWebView *view, WebKitLoadEvent load_event,
                 gpointer user_data)
{
  NomadWebViewPrivate *priv = nomad_web_view_get_instance_private (view);
  scm_call_2 (scm_c_public_ref ("emacsy emacsy", "set-buffer-name!"),
              scm_from_locale_string (
                  webkit_web_view_get_uri (WEBKIT_WEB_VIEW (view))),
              priv->buffer);
}

static void
nomad_web_view_init (NomadWebView *self)
{
  self->priv = nomad_web_view_get_instance_private (self);
  // signals
  g_signal_connect (self, "load-changed", G_CALLBACK (load_changed_cb), NULL);
  g_signal_connect (self, "decide-policy", G_CALLBACK (decide_policy_cb),
                    NULL);
}

static void
nomad_web_view_class_init (NomadWebViewClass *class)
{
}

GtkWidget *
nomad_web_view_new (WebKitSettings *settings)
{
  return g_object_new (NOMAD_WEB_VIEW_TYPE, "settings", settings, NULL);
}

void
nomad_web_view_switch_to_buffer (NomadWebView *view)
{
  scm_call_1 (scm_c_public_ref ("nomad buffer", "switch-if-not-current"),
              view->priv->buffer);
}

SCM_DEFINE (
    scm_nomad_webkit_new, "webkit-new", 2, 0, 0, (SCM buffer, SCM settings),
    "Returns a newly initialized webkit view with its parent buffer as BUFFER")
{
  GtkWidget *view = nomad_web_view_new (scm_to_pointer (settings));
  NomadWebViewPrivate *priv
      = nomad_web_view_get_instance_private (NOMAD_WEB_VIEW (view));

  priv->buffer = buffer;

  return scm_from_pointer (view, NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_reload, "webkit-reload", 1, 0, 0,
                   (SCM pointer), "Reloads the webkit POINTER uri")
{

  GtkWidget *view = scm_to_pointer (pointer);
  webkit_web_view_reload (WEBKIT_WEB_VIEW (view));
  return SCM_UNDEFINED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_uri, "webkit-uri", 1, 0, 0, (SCM pointer),
                   "Returns the current uri for a webkit view pointer. If "
                   "webview has not uri it returns #f")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  const char *uri = webkit_web_view_get_uri (WEBKIT_WEB_VIEW (view));
  if (uri)
    {
      return scm_from_locale_string (uri);
    }
  return scm_from_utf8_string ("NULL");
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_load_uri, "webkit-load-uri", 2, 0, 0,
                   (SCM pointer, SCM uri),
                   "Requests webkit VIEW pointer to load URI")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  char *c_uri = scm_to_locale_string (uri);
  webkit_web_view_load_uri (WEBKIT_WEB_VIEW (view), c_uri);
  g_free (c_uri);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_load_html, "webkit-load-html", 2, 0, 0,
                   (SCM pointer, SCM html),
                   "Requests webkit VIEW pointer to load HTML")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  char *c_content = scm_to_locale_string (html);
  webkit_web_view_load_html (WEBKIT_WEB_VIEW (view), c_content, "nomad://");
  g_free (c_content);
  return SCM_UNSPECIFIED;
}

gboolean
scroll_up_invoke (void *data)
{
  WebKitWebView *view = (WebKitWebView *)data;

  webkit_web_view_run_javascript (view, "window.scrollBy(0, -25)", NULL, NULL,
                                  NULL);
  return FALSE;
}

gboolean
scroll_down_invoke (void *data)
{
  WebKitWebView *view = (WebKitWebView *)data;

  webkit_web_view_run_javascript (view, "window.scrollBy(0, 25)", NULL, NULL,
                                  NULL);
  return FALSE;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_scroll_up, "webkit-scroll-up", 1, 0, 0,
                   (SCM pointer),
                   "Internal procedure to scroll WebView POINTER up")
{
  g_main_context_invoke (NULL, scroll_up_invoke, scm_to_pointer (pointer));
  return SCM_UNDEFINED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_scroll_down, "webkit-scroll-down", 1, 0, 0,
                   (SCM pointer),
                   "Internal procedure to scroll WebView POINTER down")
{
  g_main_context_invoke (NULL, scroll_down_invoke, scm_to_pointer (pointer));
  return SCM_UNDEFINED;
}

// FIXME: invoke on main thread?
SCM_DEFINE_PUBLIC (scm_nomad_webkit_foward, "webkit-forward", 1, 0, 0,
                   (SCM pointer),
                   "Request WebKitView POINTER to go forward in history. ")

{
  WebKitWebView *view = scm_to_pointer (pointer);
  webkit_web_view_go_forward (view);
  return SCM_BOOL_T;
}

// FIXME: invoke on main thread?
SCM_DEFINE_PUBLIC (scm_nomad_webkit_back, "webkit-back", 1, 0, 0,
                   (SCM pointer),
                   "Request WebKitView POINTER to go back in history. ")

{
  WebKitWebView *view = scm_to_pointer (pointer);
  webkit_web_view_go_back (view);
  return SCM_BOOL_T;
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
SCM_DEFINE_PUBLIC (scm_nomad_webkit_hints, "webkit-hints", 1, 0, 0,
                   (SCM pointer), "Shows WebView html links for POINTER")
{
  WebKitWebView *view = (WebKitWebView *)scm_to_pointer (pointer);

  webkit_web_view_run_javascript_from_gresource (
      view, "/org/gnu/nomad/hints.js", NULL, run_hints_cb, view);

  return SCM_UNDEFINED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_find, "webkit-find", 2, 0, 0,
                   (SCM pointer, SCM text),
                   "Finds TEXT string in  webview POINTER page")
{
  WebKitWebView *view = (WebKitWebView *)scm_to_pointer (pointer);
  WebKitFindController *controller
      = webkit_web_view_get_find_controller (view);

  webkit_find_controller_search (controller, scm_to_locale_string (text),
                                 WEBKIT_FIND_OPTIONS_CASE_INSENSITIVE, -1);
  return SCM_UNDEFINED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_find_finish, "webkit-find-finish", 1, 0, 0,
                   (SCM pointer, SCM text),
                   "Finishes the current find for webview POINTER")
{
  WebKitWebView *view = (WebKitWebView *)scm_to_pointer (pointer);
  WebKitFindController *controller
      = webkit_web_view_get_find_controller (view);

  webkit_find_controller_search_finish (controller);
  return SCM_UNDEFINED;
}

void
nomad_webkit_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkit.x"
#endif
}
