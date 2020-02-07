/*
 * ext.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

 * This file is part of Nomad

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

#include <libguile.h>
#include <webkit2/webkit-web-extension.h>

static void
message_view_okay (WebKitWebPage *web_page)
{
  webkit_web_page_send_message_to_view (
      web_page, webkit_user_message_new ("OK", NULL), NULL, NULL, NULL);
}

gboolean
web_page_message_received_db (WebKitWebPage *web_page,
                              WebKitUserMessage *message, gpointer user_data)
{
  const char *name = webkit_user_message_get_name (message);
  WebKitFrame *frame = webkit_web_page_get_main_frame (web_page);
  JSCContext *context = webkit_frame_get_js_context (frame);

  if (g_strcmp0 (name, "show-hints") == 0)
    {
      JSCValue *value = jsc_context_evaluate (context, "hintMode(false);", -1);

      message_view_okay (web_page);

      g_object_unref (value);
      g_object_ref (message);
      return TRUE;
    }
  if (g_strcmp0 (name, "hints-finish") == 0)
    {
      JSCValue *value = jsc_context_evaluate (context, "removeHints();", -1);
      g_object_unref (value);
      g_object_ref (message);
      return TRUE;
    }

  return FALSE;
}

static void
web_page_created_cb (WebKitWebExtension *extension, WebKitWebPage *web_page,
                     gpointer user_data)
{
  g_signal_connect (web_page, "user-message-received",
                    G_CALLBACK (web_page_message_received_db), NULL);
}

static void
window_object_cleared_cb (WebKitScriptWorld *world, WebKitWebPage *page,
                          WebKitFrame *frame, gpointer user_data)
{
  JSCContext *js_context = NULL;
  GBytes *bytes = NULL;
  JSCValue *result = NULL;
  const char *data;
  gsize data_size;

  js_context = webkit_frame_get_js_context_for_script_world (frame, world);
  bytes = g_resources_lookup_data ("/org/gnu/nomad/hints.js",
                                   G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);

  data = g_bytes_get_data (bytes, &data_size);
  result = jsc_context_evaluate_with_source_uri (
      js_context, data, data_size, "resource:///org/gnu/nomad/hints.js", 1);
  g_object_unref (result);
}

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)

{
  g_signal_connect (extension, "page-created",
                    G_CALLBACK (web_page_created_cb), NULL);

  g_signal_connect (webkit_script_world_get_default (),
                    "window-object-cleared",
                    G_CALLBACK (window_object_cleared_cb), NULL);
}
