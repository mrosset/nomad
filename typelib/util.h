/*
 * util.h
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

#ifndef __NOMAD_UTIL_H
#define __NOMAD_UTIL_H

#include <gtk/gtk.h>
#include <vte/vte.h>
#include <webkit2/webkit2.h>

void nomad_app_run_javascript (WebKitWebView *view, const char *js);
void nomad_app_set_style (GtkWidget *widget, const char *style);
void nomad_app_send_message (WebKitWebView *view, WebKitUserMessage *message);
gboolean nomad_draw_border (GtkWidget *widget, cairo_t *cr);
void nomad_set_wrap_mode (GtkTextView *view, gboolean wrap_mode);
void nomad_spawn_terminal (GtkWidget *terminal, const char *shell);

#endif
