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

#include <glib.h>
#include <libguile.h>

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
