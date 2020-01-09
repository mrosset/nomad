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

#ifndef __NOMADAPPFRAME_H
#define __NOMADAPPFRAME_H

#include "app.h"
#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define NOMAD_APP_FRAME_TYPE (nomad_app_frame_get_type ())

G_DECLARE_FINAL_TYPE (NomadAppFrame, nomad_app_frame, NOMAD, APP_FRAME,
		      GtkApplicationWindow)

NomadAppFrame *nomad_app_frame_new (NomadApp *app);

/**
 * nomad_app_frame_get_notebook:
 *
 * Returns: (transfer full):
 */

GtkNotebook *nomad_app_frame_get_notebook (NomadAppFrame *self);

GtkWidget *nomad_app_frame_get_minipopup (NomadAppFrame *self);

/**
 * nomad_app_frame_get_readline:
 *
 * Returns: (transfer full):
 */

void nomad_app_frame_setup_keypress (GtkApplicationWindow *window);

GtkWidget *nomad_app_frame_get_readline (NomadAppFrame *self);

void nomad_app_frame_show_minipopup (NomadAppFrame *self);

void nomad_app_frame_hide_minipopup (NomadAppFrame *self);

#endif /* __NOMADAPPWIN_H */
