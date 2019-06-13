/*
 * minibuffer.c
 * Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>
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

#include "app.h"
#include "window.h"
#include <gtk/gtk.h>
#include <libguile.h>

SCM_DEFINE (scm_nomad_minibuffer_select_line, "select-line", 1, 0, 0,
            (SCM offset), "Select minitbuffer row by offset")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *box = nomad_app_window_get_minipopup (win);
  GtkListBoxRow *row = gtk_list_box_get_row_at_index (GTK_LIST_BOX (box),
                                                      scm_to_int (offset));

  gtk_list_box_select_row (GTK_LIST_BOX (box), row);
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_minibuffer_render_popup, "render-popup", 3, 0, 0,
            (SCM proc, SCM lst, SCM selection),
            "Renders the current popup state")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *popup = nomad_app_window_get_minipopup (win);
  SCM view;
  char *c_view;

  scm_dynwind_begin (0);
  view = scm_call_2 (proc, lst, selection);
  c_view = scm_to_locale_string (view);
  webkit_web_view_load_html (WEBKIT_WEB_VIEW (popup), c_view, "nomad://");

  gtk_widget_show (popup);
  scm_dynwind_free (c_view);
  scm_dynwind_end ();
  return SCM_BOOL_T;
}

void
nomad_minibuffer_register_functions (void *data)
{
#include "minibuffer.x"
  scm_c_export ("next-line", "previous-line", "render-popup", NULL);
}
