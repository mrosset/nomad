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

#include <gtk/gtk.h>
#include <libguile.h>
#include <webkit2/webkit2.h>

#define NOMAD_APP_TYPE (nomad_app_get_type ())
G_DECLARE_FINAL_TYPE (NomadApp, nomad_app, NOMAD, APP, GtkApplication)

NomadApp *nomad_app_new ();
void nomad_app_run ();
NomadApp *nomad_app_get_default ();
GtkWindow *nomad_app_get_frame ();
void nomad_app_register_function (void *data);
#endif /* __NOMADAPP_H */
