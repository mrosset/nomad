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

SCM_DEFINE (scm_nomad_minibuffer_render_popup, "render-popup", 3, 1, 0,
            (SCM view_proc, SCM lst, SCM selection, SCM proc),
            "Renders the current popup state")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *popup = nomad_app_window_get_minipopup (win);
  SCM view;
  char *c_view;

  scm_dynwind_begin (0);
  view = scm_call_2 (view_proc, lst, selection);
  c_view = scm_to_locale_string (view);
  webkit_web_view_load_html (WEBKIT_WEB_VIEW (popup), c_view, "nomad://");

  gtk_widget_show (popup);
  scm_dynwind_free (c_view);
  scm_dynwind_end ();
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_minibuffer_whichkey_popup, "which-key-popup", 1, 0, 0,
            (SCM keymap), "Show whichkey popup for 'KEY MAP")
{
  SCM view;
  GtkWidget *mini;
  NomadAppWindow *win;

  win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  view = scm_c_public_ref ("nomad views", "which-key-view");
  mini = nomad_app_window_get_minipopup (win);

  nomad_app_window_set_keymap (win, keymap);
  scm_nomad_minibuffer_render_popup (view, keymap, scm_from_int (0),
                                     SCM_BOOL_F);
  gtk_widget_grab_focus (mini);
  return SCM_UNDEFINED;
}

SCM_DEFINE (
    scm_nomad_minibuffer_focus, "execute-extended-command", 0, 0, 0, (),
    "Moves input focus to the minibuffer shows completion for commands")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *readline = nomad_app_window_get_readline (win);
  GtkWidget *popup = nomad_app_window_get_minipopup (win);

  gtk_widget_grab_focus (readline);
  gtk_widget_show (popup);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_minibuffer_popup_hide, "hide-minibuffer-popup", 0, 0, 0,
            (), "Hides the minibuffer popup")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *popup = nomad_app_window_get_minipopup (win);
  gtk_widget_hide (popup);
  return SCM_UNSPECIFIED;
}
void
nomad_minibuffer_register_functions (void *data)
{
#include "minibuffer.x"
  scm_c_export ("next-line", "previous-line", "render-popup",
                "hide-minibuffer-popup", "execute-extended-command",
                "which-key-popup", NULL);
}
