/*
 * scm.c
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 *
 * nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * nomad is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <gtk/gtk.h>
#include <libguile.h>
#include <webkit2/webkit2.h>

#include "app.h"
#include "scheme.h"
#include "window.h"

#define NOMAD_VERSION "0.1"

SCM_DEFINE (scm_nomad_version, "nomad-version", 0, 0, 0, (), "test macro")
{
  return scm_from_utf8_string (NOMAD_VERSION);
}

SCM_DEFINE (scm_nomad_start, "browser-start", 0, 0, 0, (), "")
{
  intmax_t status;
  app = G_APPLICATION (nomad_app_new ());
  status = g_application_run (app, 0, NULL);
  return scm_from_intmax (status);
}

SCM_DEFINE (scm_nomad_kill, "kill-nomad", 0, 0, 0, (), "test macro")
{
  g_application_quit (G_APPLICATION (app));
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_webkit_load_uri, "web-view-load-uri", 1, 0, 0, (SCM uri),
            "TODO: document this procedure.")
{
  char *c_uri;
  WebKitWebView *web_view;

  scm_dynwind_begin (0);

  c_uri = scm_to_locale_string (uri);
  scm_dynwind_unwind_handler (free, c_uri, SCM_F_WIND_EXPLICITLY);

  web_view = nomad_app_get_webview (NOMAD_APP (app));

  if (web_view == NULL)
    {
      scm_dynwind_end ();
      return SCM_BOOL_F;
    }

  webkit_web_view_load_uri (web_view, c_uri);
  scm_dynwind_end ();

  return uri;
}

static void
ignore_result_cb (GObject *source_object, GAsyncResult *res,
                  gpointer user_data)
{
}

void *
scroll_down (void *data)
{
  WebKitWebView *web_view;
  web_view = nomad_app_get_webview (NOMAD_APP (app));
  webkit_web_view_run_javascript (web_view, "window.scrollBy(0, 25)", NULL,
                                  ignore_result_cb, NULL);
  return NULL;
}

SCM_DEFINE (scm_nomad_scroll_down, "scroll-down", 0, 0, 0, (), "test macro")
{
  scm_with_guile (scroll_down, NULL);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_scroll_up, "scroll-up", 0, 0, 0, (),
            "Internal procedure to the scroll WebView up")
{
  WebKitWebView *web_view;
  web_view = nomad_app_get_webview (NOMAD_APP (app));
  webkit_web_view_run_javascript (web_view, "window.scrollBy(0, -25)", NULL,
                                  NULL, NULL);
  return SCM_UNDEFINED;
}

SCM_DEFINE (
    scm_nomad_webkit_go_back, "web-view-go-back", 0, 0, 0, (),
    "Internal request WebKitView to go back in history. If WebView can not \
be found or there is no back history then it returns #f. Otherwise \
it returns #t. TODO: maybe provide a callback for load-change signal.")
{
  WebKitWebView *web_view;

  web_view = nomad_app_get_webview (NOMAD_APP (app));

  if (web_view == NULL)
    {
      return SCM_BOOL_F;
    }

  if (!webkit_web_view_can_go_back (web_view))
    {
      return SCM_BOOL_F;
    }
  webkit_web_view_go_back (web_view);
  return SCM_BOOL_T;
}

SCM_DEFINE (
    scm_nomad_webkit_go_foward, "web-view-go-forward", 0, 0, 0, (),
    "Internal request WebKitView to go forward in history. If WebView can \
not be found or there is no forward history then it returns \
#f. Otherwise it returns #t. TODO: maybe provide a callback for \
load-change signal.")
{
  WebKitWebView *web_view;

  web_view = nomad_app_get_webview (NOMAD_APP (app));

  if (web_view == NULL)
    {
      return SCM_BOOL_F;
    }

  if (!webkit_web_view_can_go_forward (web_view))
    {
      return SCM_BOOL_F;
    }
  webkit_web_view_go_forward (web_view);
  return SCM_BOOL_T;
}

SCM_DEFINE (
    scm_nomad_webkit_reload, "web-view-reload", 0, 1, 0, (SCM nocache),
    "Internally reloads WebKitView, if nocache is #t then bypass WebKit \
cache. This procedure should almost never be called directly. TODO: \
detail higher level procedures for reloading webkit. Probably only \
(reload) in this case.")
{
  WebKitWebView *web_view;

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

SCM_DEFINE (scm_nomad_get_current_url, "current-url", 0, 0, 0, (),
            "Return's the WebView's current URL. This calls webkit's \
webkit_web_view_get_uri. Note: this function can potentially return a \
URI that is not a URL. Since the API is directed towards end users, \
we use URL since it's the more common term. \
\
see https://danielmiessler.com/study/url-uri/ on the distinction of     \
URI vs URL")
{
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
register_functions (void *data)
{
#include "scheme.x"
  scm_c_export ("kill-nomad", NULL);
  return;
}
