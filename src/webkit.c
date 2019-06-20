/*
 * webkit.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Nomad
 *
 * Nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   Nomad is distributed in the hope that it will be useful, but
 *   WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *   See the GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "app.h"
#include "request.h"
#include "util.h"

SCM_DEFINE (scm_nomad_webkit_load_uri, "webview-load-uri", 1, 0, 0, (SCM uri),
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

gboolean
scroll_up_invoke (void *data)
{
  WebKitWebView *web_view;
  web_view = nomad_app_get_webview (NOMAD_APP (app));
  webkit_web_view_run_javascript (web_view, "window.scrollBy(0, -25)", NULL,
                                  NULL, NULL);
  return FALSE;
}

SCM_DEFINE (scm_nomad_scroll_up, "scroll-up", 0, 0, 0, (),
            "Internal procedure to scroll WebView up")
{
  g_main_context_invoke (NULL, scroll_up_invoke, NULL);
  return SCM_UNDEFINED;
}

gboolean
scroll_down_invoke (void *data)
{
  WebKitWebView *web_view;
  web_view = nomad_app_get_webview (NOMAD_APP (app));
  webkit_web_view_run_javascript (web_view, "window.scrollBy(0, 25)", NULL,
                                  NULL, NULL);
  return FALSE;
}

SCM_DEFINE (scm_nomad_scroll_down, "scroll-down", 0, 0, 0, (), "")
{
  g_main_context_invoke (NULL, scroll_down_invoke, NULL);
  return SCM_UNDEFINED;
}

gboolean
web_view_back_invoke (void *data)
{
  struct request *request = data;

  WebKitWebView *web_view = nomad_app_get_webview (app);

  if (web_view == NULL)
    {
      request->done = TRUE;
      return FALSE;
    }

  if (!webkit_web_view_can_go_back (web_view))
    {
      request->done = TRUE;
      return FALSE;
    }
  webkit_web_view_go_back (web_view);
  request->response = SCM_BOOL_T;
  request->done = TRUE;
  return FALSE;
}

SCM_DEFINE (
    scm_nomad_webkit_go_back, "webview-go-back", 0, 0, 0, (),
    "Request WebKitView to go back in history. If WebView can not be found or "
    "there is no back history then it return #f. Otherwise it returns #t.")
{
  struct request *request
      = &(struct request){ .response = SCM_BOOL_F, .done = FALSE };

  g_main_context_invoke (NULL, web_view_back_invoke, request);
  wait_for_response (request);
  return request->response;
}

// FIXME: invoke on main thread
SCM_DEFINE (
    scm_nomad_webkit_go_foward, "webview-go-forward", 0, 0, 0, (),
    "Internal request WebKitView to go forward in history. If WebView can not "
    "be found or there is no forward history then it returns #f. Otherwise it "
    "returns #t. TODO: maybe provide a callback for load-change signal.")
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

SCM_DEFINE (scm_nomad_webkit_reload, "webview-reload", 0, 1, 0, (SCM nocache),
            "Internally reloads WebKitView, if nocache is #t then bypass "
            "WebKit cache. This procedure should almost never be called "
            "directly. TODO: detail higher level procedures for reloading "
            "webkit. Probably only (reload) in this case.")
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

SCM_DEFINE (scm_nomad_get_current_url, "webview-current-url", 0, 0, 0, (),
            "Return's the WebView's current URL. This calls webkit's \
webkit_web_view_get_uri. Note: this function can potentially return a   \
URI that is not a URL. Since the API is directed towards end users,     \
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

SCM_DEFINE (scm_make_web_view, "make-web-view", 0, 0, 0, (),
            "Creates and returns a pointer to a new webkit view.")
{
  WebKitWebView *a_web_view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  a_web_view = g_object_ref (a_web_view);
  return scm_from_pointer (a_web_view, /*g_free*/ NULL);
}

void
nomad_webkit_register_functions (void *data)
{
#include "webkit.x"
  scm_c_export ("webview-load-uri", "webview-go-back", "webview-go-forward",
                "webview-reload", "webview-current-url", "scroll-up",
                "scroll-down", "make-web-view", NULL);
}
