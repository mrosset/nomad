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

void
inner_main (void *data, int argc, char **argv)
{
  intmax_t status;
  app = G_APPLICATION (wemacs_app_new ());
  status = g_application_run (app, argc, argv);
  exit (status);
}

int
main (int argc, char *argv[])
{
  scm_init_guile ();
  scm_c_use_module ("wemacs init");
  scm_c_define_module ("wemacs browser", register_functions, NULL);
  scm_c_eval_string ("(init)");
}
