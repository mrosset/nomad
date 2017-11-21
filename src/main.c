/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-  */
/*
 * main.c
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 *
 * wemacs is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * wemacs is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <gtk/gtk.h>
#include <libguile.h>

#include "app.h"
#include "scheme.h"

int
main (int argc, char *argv[])
{
  scm_init_guile ();
  // scm_with_guile (&register_functions, NULL);
  scm_c_primitive_load (WEMACS_SCHEME_INIT);
  scm_c_use_module ("wemacs browser");
  scm_c_define_module ("wemacs browser", register_functions, NULL);
  scm_shell (argc, argv);
}
