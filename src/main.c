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

#include "app.h"
#include "buffer.h"
#include "emacsy.h"
#include "minibuffer.h"
#include "util.h"
#include "webview.h"
#include "window.h"
#include <gtk/gtk.h>
#include <libguile.h>
#include <libguile/hooks.h>

static void
startup (GApplication *app, gpointer data)
{
}

static void
shutdown (GApplication *app, gpointer data)
{
  scm_call_0 (scm_c_public_ref ("nomad app", "shutdown"));
}

static void
register_c_modules ()
{
  // Modules that are used before defining have a scheme file. This
  // allows mixing pure scheme with C scheme.
  scm_c_use_module ("nomad app");
  scm_c_define_module ("nomad app", nomad_app_register_functions, app);

  scm_c_use_module ("nomad buffer");
  scm_c_define_module ("nomad buffer", nomad_buffer_register_functions, NULL);

  scm_c_use_module ("nomad webview");
  scm_c_define_module ("nomad webview", nomad_webview_register_functions,
                       NULL);

  scm_c_define_module ("nomad window", nomad_window_register_functions, NULL);

  scm_c_use_module ("nomad minibuffer");
  scm_c_define_module ("nomad minibuffer", nomad_minibuffer_register_functions,
                       NULL);

  scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad init");
  scm_c_use_module ("emacsy emacsy");
  scm_c_use_module ("nomad util");
  scm_c_use_module ("nomad window");
  scm_c_use_module ("nomad views");
  scm_c_use_module ("nomad options");
  scm_c_use_module ("nomad repl");
}

static void
inner_main (void *data, int argc, char **argv)
{
  int err;

  err = emacsy_initialize (EMACSY_INTERACTIVE);
  if (err)
    exit (err);

  register_c_modules ();

  app = nomad_app_new ();

  // App signals
  g_signal_connect (app, "startup", G_CALLBACK (startup), NULL);
  g_signal_connect (app, "shutdown", G_CALLBACK (shutdown), NULL);
  // We need to call init for so things like GDK_SCALE are used by our
  // GApplication
  scm_call_0 (scm_c_public_ref ("nomad init", "init"));
  exit (g_application_run (G_APPLICATION (app), argc, argv));
}

static void
init_environment ()
{
  const char *env_ccache;
  char *ccache;

  // If GUILE_LOAD_COMPILED_PATH is set. Append Nomad's site-ccache to
  // the environment
  env_ccache = g_getenv ("GUILE_LOAD_COMPILED_PATH");
  if (env_ccache)
    {
      ccache = g_strconcat (env_ccache, ":", NOMAD_GUILE_LOAD_COMPILED_PATH,
                            NULL);
    }
  else
    {
      ccache = NOMAD_GUILE_LOAD_COMPILED_PATH;
    }
  g_setenv ("GUILE_LOAD_COMPILED_PATH", ccache, TRUE);
}

int
main (int argc, char *argv[])
{
  /* init_environment (); */
  scm_boot_guile (argc, argv, inner_main, NULL);
}
