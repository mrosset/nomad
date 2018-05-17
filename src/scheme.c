/*
 * scheme.c
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
  app = nomad_app_new ();
  status = g_application_run (G_APPLICATION (app), 0, NULL);
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

gboolean
web_view_back_invoke (void *data)
{
  WebKitWebView *web_view = nomad_app_get_webview (app);

  if (web_view == NULL)
    {
      return FALSE;
    }

  if (!webkit_web_view_can_go_back (web_view))
    {
      return FALSE;
    }
  webkit_web_view_go_back (web_view);
  return FALSE;
}

SCM_DEFINE (scm_nomad_webkit_go_back, "web-view-go-back", 0, 0, 0, (),
            "Request WebKitView to go back in history. If WebView can not \
be found or there is no back history then it returns #f. Otherwise      \
it returns #t.")
{
  g_main_context_invoke (NULL, web_view_back_invoke, NULL);
  return SCM_UNDEFINED;
}

SCM_DEFINE (
    scm_nomad_webkit_go_foward, "web-view-go-forward", 0, 0, 0, (),
    "Internal request WebKitView to go forward in history. If WebView can \
not be found or there is no forward history then it returns             \
#f. Otherwise it returns #t. TODO: maybe provide a callback for         \
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
cache. This procedure should almost never be called directly. TODO:     \
detail higher level procedures for reloading webkit. Probably only      \
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
scheme_test_invoke (void *data)
{
  return FALSE;
}

SCM_DEFINE (scm_nomad_scheme_test, "scheme-test", 0, 0, 0, (), "")
{
  struct buffer *buf;
  buf = (struct buffer *)scm_gc_malloc (sizeof (struct buffer), "buffer");
  g_main_context_invoke (NULL, scheme_test_invoke, buf);
  return scm_make_foreign_object_1 (buffer_type, buf);
}

gboolean
make_buffer_invoke (void *data)
{
  char *uri = data;
  NomadBuffer *buf = nomad_buffer_new ();
  WebKitWebView *view = nomad_buffer_get_view (buf);
  GtkWidget *win = nomad_app_get_window (NOMAD_APP (app));
  webkit_web_view_load_uri (view, uri);
  nomad_app_window_set_buffer (NOMAD_APP_WINDOW (win), buf);
  nomad_app_add_buffer (NOMAD_APP (app), buf);
  g_free (uri);

  return FALSE;
}

SCM_DEFINE (scm_nomad_current_buffer, "current-buffer", 0, 0, 0, (), "")
{
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_make_buffer, "make-buffer", 0, 1, 0, (SCM uri), "")
{
  char *c_uri = scm_to_locale_string (uri);
  g_main_context_invoke (NULL, make_buffer_invoke, c_uri);
  return SCM_UNDEFINED;
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

void
register_functions (void *data)
{
  scm_c_define_module ("nomad app", nomad_app_register_functions, data);
  nomad_app_register_functions (data);
#include "scheme.x"
  init_buffer_type ();
  scm_c_export ("kill-nomad", "make-buffer", "buffer-list", "current-buffer",
                "next-buffer", "prev-buffer", "scheme-test", NULL);
  return;
}
