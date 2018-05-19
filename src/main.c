/*
 * main.c
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

#include <gtk/gtk.h>
#include <libguile.h>

#include "app.h"
#include "buffer.h"
#include "webkit.h"

void
inner_main (void *data, int argc, char **argv)
{
  intmax_t status;

  app = nomad_app_new ();

  // init scheme C modules
  scm_c_define_module ("nomad app", nomad_app_register_functions, app);
  scm_c_define_module ("nomad webkit", nomad_webkit_register_functions, NULL);
  scm_c_define_module ("nomad buffer-internal",
                       nomad_buffer_register_functions, NULL);

  scm_c_use_module ("nomad app");
  scm_c_use_module ("nomad webkit");
  scm_c_use_module ("nomad buffer");
  scm_c_use_module ("nomad buffer-internal");
  scm_c_use_module ("nomad init");
  scm_c_use_module ("nomad tests");

  scm_c_eval_string ("(init)");

  status = g_application_run (G_APPLICATION (app), argc, argv);
  exit (status);
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);
}
