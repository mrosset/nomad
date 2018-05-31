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
#include "util.h"
#include "webkit.h"
#include "window.h"

static void
startup (GApplication *app, gpointer data)
{
  // Define scheme C modules
  // Modules that are used before defining have a scheme file. This
  // allows mixing pure scheme with C scheme.
  scm_c_use_module ("nomad webkit");
  scm_c_define_module ("nomad webkit", nomad_webkit_register_functions, NULL);

  scm_c_define_module ("nomad window", nomad_window_register_functions, NULL);

  scm_c_use_module ("nomad buffer");
  scm_c_define_module ("nomad buffer", nomad_buffer_register_functions, NULL);

  scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad util");
  scm_c_use_module ("nomad window");
  scm_c_use_module ("nomad browser");
  scm_c_use_module ("nomad repl");

  // FIXME: users can start REPL via user-init-hook in $HOME/.nomad. Add
  // documentation for $HOME/.nomad
  scm_c_run_hook (scm_c_public_ref ("nomad init", "user-init-hook"),
                  SCM_LIST0);
  scm_c_eval_string ("(server-start-coop)");
}

static void
shutdown (GApplication *app, gpointer data)
{
  scm_c_eval_string ("(server-force-delete)");
}

void
inner_main (void *data, int argc, char **argv)
{
  app = nomad_app_new ();

  // App signals
  g_signal_connect (app, "startup", G_CALLBACK (startup), NULL);
  g_signal_connect (app, "shutdown", G_CALLBACK (shutdown), NULL);

  scm_c_use_module ("nomad app");
  scm_c_define_module ("nomad app", nomad_app_register_functions, app);

  // Set emacs-init-file to datadir installed file
  scm_variable_set_x (scm_c_lookup ("emacs-init-file"),
                      scm_from_locale_string (NOMAD_DATAROOT_DIR "/init.el"));

  // We need to call init for so things like GDK_SCALE are used by our
  // GApplication
  scm_c_use_module ("nomad init");
  scm_c_eval_string ("(init)");

  exit (g_application_run (G_APPLICATION (app), argc, argv));
}

int
main (int argc, char *argv[])
{
  // FIXME: this clobbers GUILE_LOAD_COMPILED_PATH we should append to
  // %load-compiled-path
  g_setenv ("GUILE_LOAD_COMPILED_PATH", NOMAD_GUILE_LOAD_COMPILED_PATH, TRUE);
  scm_boot_guile (argc, argv, inner_main, NULL);
}
