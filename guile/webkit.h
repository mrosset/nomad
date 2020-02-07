/*
 * webkit.h
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

#ifndef __GUILE_NOMAD_WEBKIT_H
#define __GUILE_NOMAD_WEBKIT_H

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define NOMAD_WEB_VIEW_TYPE (nomad_web_view_get_type ())
G_DECLARE_FINAL_TYPE (NomadWebView, nomad_web_view, NOMAD, WEB_VIEW,
                      WebKitWebView)

GtkWidget *nomad_web_view_new ();
void nomad_web_view_switch_to_buffer (NomadWebView *view);
void nomad_webkit_register_function (void *data);

void nomad_app_run_javascript (WebKitWebView *view, const char *js);
void nomad_app_send_message (WebKitWebView *view, WebKitUserMessage *message);
void nomad_app_set_webview_signals (WebKitWebView *view);

#endif
