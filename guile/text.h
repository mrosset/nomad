/*
 * text.h
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

#ifndef __GUILE_NOMAD_TEXT_H
#define __GUILE_NOMAD_TEXT_H

#include <gtksourceview/gtksource.h>

GtkSourceBuffer *nomad_app_source_buffer_new (const char *theme,
                                              const char *lang);

void nomad_app_source_view_set_buffer (GtkTextView *view, const char *theme,
                                       const char *lang);

void nomad_text_register_function (void *data);
SCM scm_nomad_source_new ();
GtkWidget *nomad_app_source_view_new ();

#endif
