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

#include "webview.h"
#include "util.h"

typedef struct _NomadWebViewPrivate NomadWebViewPrivate;

struct _NomadWebViewPrivate
{
  /* SCM buffer; */
  const char *field;
};

struct _NomadWebView
{
  WebKitWebView parent;
  NomadWebViewPrivate *priv;
};

// clang-format off
G_DEFINE_TYPE_WITH_PRIVATE (NomadWebView, nomad_web_view, WEBKIT_TYPE_WEB_VIEW);
// clang-format on

/* static void
 * noamd_app_send_message_ready_cb (GObject *view, GAsyncResult *result,
 *                                  gpointer user_data)
 * {
 *   GVariant *parameters;
 *   WebKitUserMessage *reply = NULL;
 *
 *   char *status = (char *)user_data;
 *   g_autoptr (GError) error = NULL;
 *
 *   reply = webkit_web_view_send_message_to_page_finish (WEBKIT_WEB_VIEW
 * (view), result, &error); if (error)
 *     {
 *       g_warning ("Error getting Message: %s\n", error->message);
 *       g_free (user_data);
 *       return;
 *     }
 *
 *   parameters = webkit_user_message_get_parameters (reply);
 *   if (!parameters)
 *     {
 *       g_free (user_data);
 *       return;
 *     }
 *   status = g_strdup (g_variant_get_string (parameters, NULL));
 *   scm_variable_set_x (
 *       scm_c_public_variable ("nomad gtk buffers", "message-reply"),
 *       scm_from_locale_string (status));
 *   g_free (status);
 * } */

static void
nomad_web_view_init (NomadWebView *self)
{
  self->priv = nomad_web_view_get_instance_private (self);
}

static void
nomad_web_view_class_init (NomadWebViewClass *class)
{
}
