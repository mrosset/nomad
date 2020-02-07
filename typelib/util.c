/*
 * util.c
 * Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>
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

#include "util.h"

enum
{
  RECEIVED,
  LAST
};

static guint web_view_signals[LAST] = { 0 };

void
nomad_app_run_javascript (WebKitWebView *view, const char *js)
{
  webkit_web_view_run_javascript (view, js, NULL, NULL, NULL);
}

void
nomad_app_set_style (GtkWidget *widget, const char *style)
{
  GtkCssProvider *provider = gtk_css_provider_new ();
  gtk_css_provider_load_from_data (provider, style, -1, NULL);
  gtk_style_context_add_provider (gtk_widget_get_style_context (widget),
                                  GTK_STYLE_PROVIDER (provider),
                                  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);
}

void
nomad_app_send_message (WebKitWebView *view, WebKitUserMessage *message)
{
  webkit_web_view_send_message_to_page (view, message, NULL, NULL, NULL);
}

gboolean
user_message_received_cb (WebKitWebView *web_view, WebKitUserMessage *message,
                          gpointer user_data)
{
  const char *name = webkit_user_message_get_name (message);
  g_signal_emit (web_view, web_view_signals[RECEIVED], 0, name);
  return TRUE;
}

void
nomad_app_set_webview_signals (WebKitWebView *view)
{
  g_signal_connect (view, "user-message-received",
                    G_CALLBACK (user_message_received_cb), NULL);

  // clang-format off
  web_view_signals[RECEIVED] =
    g_signal_new("message-received",
		 WEBKIT_TYPE_WEB_VIEW,
		 G_SIGNAL_RUN_FIRST,
		 0, NULL, NULL,
		 g_cclosure_marshal_VOID__POINTER,
		 G_TYPE_NONE, 1, G_TYPE_STRING);
  // clang-format on
}
