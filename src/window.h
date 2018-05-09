/*
 * nomadappwin.h
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
#ifndef __NOMADAPPWIN_H
#define __NOMADAPPWIN_H

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#include "app.h"

#define DEFAULT_URI "https://duckduckgo.com/html"

#define NOMAD_APP_WINDOW_TYPE (nomad_app_window_get_type ())
G_DECLARE_FINAL_TYPE (NomadAppWindow, nomad_app_window, NOMAD, APP_WINDOW,
                      GtkApplicationWindow)
NomadAppWindow *nomad_app_window_new (NomadApp *app);
WebKitWebView *nomad_app_window_get_webview (NomadAppWindow *win);
GtkWidget *nomad_app_window_get_box(NomadAppWindow *win);
GtkWidget *nomad_app_window_get_status(NomadAppWindow *win);
void nomad_app_window_set_webview(NomadAppWindow *win, WebKitWebView *view);
void nomad_app_window_replace_webview(NomadAppWindow *win, WebKitWebView *view);
#endif /* __NOMADAPPWIN_H */
