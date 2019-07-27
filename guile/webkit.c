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
#include "util.h"
#include "app.h"
/* #include <glib-object.h> */
#include <libguile.h>
#include <webkit2/webkit2.h>

typedef struct _NomadWebViewPrivate NomadWebViewPrivate;

struct _NomadWebViewPrivate
{
  SCM buffer;
  GtkWidget *view;
};

struct _NomadWebView
{
  GObject parent;
  NomadWebViewPrivate *priv;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadWebView, nomad_web_view, G_TYPE_OBJECT);

static void
nomad_web_view_init (NomadWebView *self)
{
  self->priv = nomad_web_view_get_instance_private (self);
  self->priv->view = webkit_web_view_new ();
}

static void
nomad_web_view_class_init (NomadWebViewClass *class)
{
}

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  scm_call_0 (scm_c_public_ref ("nomad webview", "webview-onload"));
}

NomadWebView *
nomad_web_view_new ()
{
  return g_object_new (NOMAD_WEB_VIEW_TYPE, NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_new, "webkit-new", 0, 0, 0, (SCM pointer),
                   "Returns a newly initialized webkit view")
{
  GtkWidget *view = webkit_web_view_new ();

  g_signal_connect (view, "load-changed", G_CALLBACK (web_view_load_changed),
                    NULL);
  return scm_from_pointer (view, NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_network_proxy_settings_new,
                   "webkit_network_proxy_settings_new", 2, 0, 0, (SCM proxy, SCM ignore),
                   "Returns a newly initialized webkit proxy settings.")
{
  size_t len = scm_to_int (scm_length (ignore));

  gchar* c_ignore[len];
  scm_to_argv(ignore,  c_ignore);
  /* FIXME: ignore is not a string but a list of strings like example.com or
     *.foo.org etc. The code above is obviously wrong. */
  /* g_free (c_proxy); */
  /* g_free (c_ignore); */
  return scm_from_pointer (webkit_network_proxy_settings_new
                           (scm_to_locale_string (proxy), (const char * const*) c_ignore),
                           NULL);
}
  return SCM_UNSPECIFIED;
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

SCM_DEFINE_PUBLIC (
    scm_nomad_webkit_reload, "webview-reload", 0, 1, 0, (SCM nocache),
    "Internally reloads WebKitView, if nocache is #t then bypass "
    "WebKit cache. This procedure should almost never be called "
    "directly. TODO: detail higher level procedures for reloading "
    "webkit. Probably only (reload) in this case.")
{
  WebKitWebView *web_view;
  NomadApp *app = nomad_app_get_default ();

  web_view = nomad_app_get_webview (NOMAD_APP (app));

  if (web_view == NULL)
    {
      return SCM_BOOL_F;
    }

  if (scm_is_true (nocache))
    {
      webkit_web_view_reload_bypass_cache (web_view);
    }
  else
    {
      webkit_web_view_reload (web_view);
    }
  return SCM_BOOL_T;
}

SCM_DEFINE_PUBLIC (
    scm_nomad_get_current_url, "webview-current-url", 0, 0, 0, (),
    "Return's the WebView's current URL. This calls webkit's "
    "webkit_web_view_get_uri. Note: this function can potentially "
    "return a URI that is not a URL. Since the API is directed "
    "towards end users, we use URL since it's the more common term, "
    "see https://danielmiessler.com/study/url-uri/ on the distinction "
    "of URI vs URL")
{
  NomadApp *app = nomad_app_get_default ();
  WebKitWebView *web_view;
  const char *uri;
  SCM result;

  web_view = nomad_app_get_webview (NOMAD_APP (app));
  uri = webkit_web_view_get_uri (web_view);

  if (uri == NULL)
    {
      result = scm_from_utf8_string ("URI not loaded");
    }
  else
    {
      result = scm_from_locale_string (uri);
    }

  return result;
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

void
nomad_webkit_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkit.x"
#endif
}
