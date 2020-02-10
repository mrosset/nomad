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
#include "../config.h"
#include <libguile.h>

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

static void
open_cb (GApplication *app, GFile **files, gint n_files, gchar *hint,
         gpointer user_data)
{
  g_print ("OPEN\n");

  if (!gtk_application_get_active_window (GTK_APPLICATION (app)))
    {
      scm_call_0 (scm_c_private_ref ("nomad platform ", "make-frame"));
      gtk_widget_show_all (GTK_WIDGET (
          gtk_application_get_active_window (GTK_APPLICATION (app))));
    }

  for (int i = 0; i < n_files; i++)
    {
      scm_call_1 (scm_c_private_ref ("nomad platform ", "make-webview-buffer"),
                  scm_from_locale_string (g_file_get_uri (files[i])));
    }
}

static void
scm_nomad_free_argv (void *data)
{
  g_strfreev (data);
}

SCM_DEFINE_PUBLIC (scm_nomad_list_to_argv, "list->argv", 1, 0, 0, (SCM lst),
                   "Converts LST to char **argv and returns SCM pointer")
{
  int len;
  gchar **argv;

  len = scm_to_int (scm_length (lst));
  argv = malloc (sizeof (char *) * len + 1);

  argv[len] = NULL;

  for (int i = 0; i < len; i++)
    {
      SCM item = scm_list_ref (lst, scm_from_int (i));
      argv[i] = scm_to_locale_string (item);
    }

  return scm_from_pointer (argv, scm_nomad_free_argv);
}

void
nomad_app_run (GtkApplication *app)
{
  SCM lst, ptr;
  int argc;
  char **argv;
  GOptionEntry entries[]
      = { { "quick", 'Q', G_OPTION_FLAG_NONE, G_OPTION_ARG_NONE, NULL,
            "Start nomad without using user-init-file", NULL },
          { NULL } };

  lst = scm_c_eval_string ("(command-line)");
  ptr = scm_nomad_list_to_argv (lst);
  argv = g_strdupv (scm_to_pointer (ptr));
  argc = scm_to_int (scm_length (lst));

  // clang-format off
  g_application_set_flags (G_APPLICATION (app),
                           G_APPLICATION_HANDLES_OPEN |
                           G_APPLICATION_CAN_OVERRIDE_APP_ID);
  // clang-format on

  g_signal_connect (app, "open", G_CALLBACK (open_cb), NULL);

  g_application_add_main_option_entries (G_APPLICATION (app), entries);

  g_application_run (G_APPLICATION (app), argc, argv);
}

// scheme
SCM_DEFINE_PUBLIC (
    scm_nomad_version, "nomad-version", 0, 0, 0, (),
    "Return string describing the version of Nomad that is running")
{
  return scm_from_utf8_string (VERSION);
}
