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

SCM_DEFINE (scm_nomad_minibuffer_next_line, "next-line", 0, 0, 0, (),
            "Move to next minibuffer line")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *box = nomad_app_window_get_minipopup (win);
  SCM line = scm_variable_ref (scm_c_lookup ("current-line"));
  SCM next = scm_sum (line, scm_from_int (1));
  GtkListBoxRow *row
      = gtk_list_box_get_row_at_index (GTK_LIST_BOX (box), scm_to_int (next));

  printf ("next-line\n");
  scm_variable_set_x (scm_c_lookup ("current-line"), next);

  gtk_list_box_select_row (GTK_LIST_BOX (box), row);

  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_minibuffer_previous_line, "previous-line", 0, 0, 0, (),
            "Move to previous minibuffer line")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *box = nomad_app_window_get_minipopup (win);
  SCM line = scm_variable_ref (scm_c_lookup ("current-line"));
  SCM next = scm_difference (line, scm_from_int (1));
  GtkListBoxRow *row
      = gtk_list_box_get_row_at_index (GTK_LIST_BOX (box), scm_to_int (next));

  scm_variable_set_x (scm_c_lookup ("current-line"), next);

  gtk_list_box_select_row (GTK_LIST_BOX (box), row);

  return SCM_BOOL_T;
}

void
nomad_minibuffer_register_functions (void *data)
{
#include "minibuffer.x"
  scm_c_export ("next-line", "previous-line", NULL);
}
