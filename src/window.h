/*
 * window.h
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

#ifndef __NOMADAPPWIN_H
#define __NOMADAPPWIN_H

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#include "app.h"

#define NOMAD_APP_WINDOW_TYPE (nomad_app_window_get_type ())

G_DECLARE_FINAL_TYPE (NomadAppWindow, nomad_app_window, NOMAD, APP_WINDOW,
                      GtkApplicationWindow)

NomadAppWindow *nomad_app_window_new (NomadApp *app);
WebKitWebView *nomad_app_window_get_webview (NomadAppWindow *win);
GtkNotebook *nomad_window_get_notebook (NomadAppWindow *win);
NomadBuffer *nomad_app_window_get_buffer (NomadAppWindow *win);
GtkWidget *nomad_app_window_get_minipopup (NomadAppWindow *win);
void nomad_app_window_add_buffer (NomadAppWindow *win, NomadBuffer *buf);
void nomad_app_window_remove_buffer (NomadAppWindow *win);
void nomad_app_window_add_vte (NomadAppWindow *win);
void nomad_app_window_grab_vte (NomadAppWindow *win);
void nomad_app_window_show_vte (NomadAppWindow *win);
void nomad_app_window_hide_vte (NomadAppWindow *win);
void nomad_app_window_start_repl (NomadAppWindow *win);
void nomad_window_register_functions (void *data);
GList *nomad_window_get_tabs (NomadAppWindow *win);
#endif /* __NOMADAPPWIN_H */
