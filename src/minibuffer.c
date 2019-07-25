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
#include "frame.h"
#include "util.h"
#include <gtk/gtk.h>
#include <libguile.h>

SCM_DEFINE (scm_nomad_minibuffer_render_popup, "render-popup", 3, 1, 0,
            (SCM view_proc, SCM lst, SCM selection, SCM proc),
            "Renders the current popup state")
{

  NomadAppFrame *frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  GtkWidget *popup = nomad_app_frame_get_minipopup (frame);
  SCM view;
  char *c_view;

  scm_dynwind_begin (0);
  view = scm_call_2 (view_proc, lst, selection);
  c_view = scm_to_locale_string (view);
  webkit_web_view_load_html (WEBKIT_WEB_VIEW (popup), c_view, "nomad://");

  nomad_app_frame_show_minipopup (frame);
  scm_dynwind_free (c_view);
  scm_dynwind_end ();
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_minibuffer_whichkey_popup, "which-key-popup", 1, 0, 0,
            (SCM keymap), "Show whichkey popup for 'KEY MAP")
{
  SCM view;
  GtkWidget *mini;
  NomadAppFrame *frame;

  frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  view = scm_c_public_ref ("nomad views", "which-key-view");
  mini = nomad_app_frame_get_minipopup (frame);

  scm_nomad_minibuffer_render_popup (view, keymap, scm_from_int (0),
                                     SCM_BOOL_F);
  gtk_widget_grab_focus (mini);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_minibuffer_popup_hide, "hide-minibuffer-popup", 0, 0, 0,
            (), "Hides the minibuffer popup")
{
  NomadAppFrame *frame = NOMAD_APP_FRAME (nomad_app_get_frame ());
  nomad_app_frame_hide_minipopup (frame);
  return SCM_UNSPECIFIED;
}
void
nomad_minibuffer_register_functions (void *data)
{
#include "minibuffer.x"
  scm_c_export ("render-popup", "hide-minibuffer-popup", "which-key-popup",
                NULL);
  scm_c_register_interactive ("hide-minibuffer-popup");
}
