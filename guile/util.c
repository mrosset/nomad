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

static void
scm_nomad_free_argv (void *data)
{
  g_strfreev (data);
}

SCM_DEFINE_PUBLIC (scm_nomad_list_to_argv, "list->argv", 1, 0, 0, (SCM lst),
                   "Converts LST to char **argv and returns SCM pointer")
{
  int len;
  gchar **argv;

  len = scm_to_int (scm_length (lst));
  argv = malloc (sizeof (char *) * len + 1);

  for (int i = 0; i < len; i++)
    {
      SCM item = scm_list_ref (lst, scm_from_int (i));
      argv[i] = scm_to_locale_string (item);
    }

  argv[len] = NULL;

  return scm_from_pointer (argv, scm_nomad_free_argv);
}

SCM_DEFINE_PUBLIC (scm_nomad_argv_to_list, "argv->list", 1, 0, 0,
                   (SCM pointer),
                   "Converts char **argv SCM POINTER to string list")
{
  char **argv = scm_to_pointer (pointer);
  int len = g_strv_length (argv);
  SCM lst = scm_call_1 (scm_c_public_ref ("guile", "make-list"),
                        scm_from_int (len));

  for (int i = 0; i < len; i++)
    {
      scm_list_set_x (lst, scm_from_int (i), scm_from_locale_string (argv[i]));
    }

  return lst;
}

SCM_DEFINE_PUBLIC (scm_nomad_argv_length, "argv-length", 1, 0, 0,
                   (SCM pointer),
                   "Returns the number of strings in argv POINTER")
{
  char **argv = scm_to_pointer (pointer);
  return scm_from_int (g_strv_length (argv));
}

// FIXME: emacsy has its own yanking use emacsy primitives here
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
