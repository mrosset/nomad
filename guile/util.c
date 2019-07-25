/*
 * util.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Nomad
 *
 * Nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   Nomad is distributed in the hope that it will be useful, but
 *   WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *   See the GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gtk/gtk.h>
#include <libguile.h>

SCM
scm_c_current_buffer ()
{
  return scm_call_0 (scm_c_public_ref ("emacsy emacsy", "current-buffer"));
}

void
scm_c_debug_object (SCM object)
{
  scm_call_1 (scm_c_public_ref ("nomad util", "debug-object"), object);
}

void
scm_to_argv (SCM list, char **argv)
{
  int len = scm_to_int (scm_length (list));
  for (int i = 0; i < len; i++)
    {
      argv[i] = scm_to_locale_string (scm_list_ref (list, scm_from_int (i)));
    }
  argv[len] = NULL;
}

SCM_DEFINE_PUBLIC (scm_nomad_yank_string, "yank-string", 1, 0, 0, (SCM string),
                   "Grabs STRING to primary clipboard")
{

  GtkClipboard *clip = gtk_clipboard_get_default (gdk_display_get_default ());
  int len = scm_to_int (scm_string_length (string));
  char *c_text = scm_to_locale_string (string);

  scm_dynwind_begin (0);
  gtk_clipboard_set_text (clip, c_text, len);
  scm_dynwind_free (c_text);
  scm_dynwind_end ();
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_c_register_interactive, "nomad-register-interactive", 1, 0, 0,
            (const char *c_name),
            "Registers procedure name as an interactive command")
{
  SCM name = scm_from_locale_string (c_name);
  SCM sym = scm_string_to_symbol (name);
  SCM proc = scm_eval_string (name);

  return scm_call_2 (
      scm_c_public_ref ("emacsy command", "register-interactive"), sym, proc);
}

SCM
scm_c_make_command (const char *key)
{
  return scm_call_1 (scm_c_public_ref ("nomad eval", "make-command"),
                     scm_string_to_symbol (scm_from_utf8_string (key)));
}

void
nomad_util_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "util.x"
#endif
}
