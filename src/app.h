/*
 * app.h
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

#ifndef __NOMADAPP_H
#define __NOMADAPP_H

#include "buffer.h"
#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define MAX_BUFFERS 1

#define NOMAD_APP_TYPE (nomad_app_get_type ())
G_DECLARE_FINAL_TYPE (NomadApp, nomad_app, NOMAD, APP, GtkApplication)

NomadApp *app;

NomadApp *nomad_app_new (void);
WebKitWebView *nomad_app_get_webview (NomadApp *app);
GtkWidget *nomad_app_get_window (NomadApp *self);
NomadBuffer *nomad_app_get_first_buffer (NomadApp *app);
SCM nomad_app_get_buffers (NomadApp *app);
GList *nomad_app_get_buffer_list (NomadApp *app);
void nomad_app_remove_buffer (NomadApp *app, NomadBuffer *buf);
void nomad_app_next_buffer (NomadApp *app);
void nomad_app_prev_buffer (NomadApp *app);
void nomad_app_register_functions (void *data);
SCM nomad_app_make_buffer (NomadBuffer *buf);
#endif /* __NOMADAPP_H */
