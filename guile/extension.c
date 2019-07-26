/*
 * extention.c
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

#include "frame.h"
#include "minibuffer.h"
#include "util.h"
#include "webkit.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>

SCM_DEFINE_PUBLIC (scm_nomad_temp, "guile-nomad-available", 0, 0, 0, (),
                   "Test function")
{
  return SCM_BOOL_T;
}

void
register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "extension.x"
#endif
}

void
init_guile_nomad ()
{
  /*
   * Define our C scheme modules.
   *
   * NOTE: Order is important here when it comes to mixing scheme and C
   * code. The scheme code needs to be used first before defining C
   * modules. And scheme modules that use C modules need to have the C module
   * defined first.
   *
   */
  scm_c_define_module ("nomad lib", register_function, NULL);
  scm_c_define_module ("nomad webkit", nomad_webkit_register_function, NULL);
  scm_c_define_module ("nomad frame", nomad_frame_register_function, NULL);

  scm_c_use_module ("nomad app");
  scm_c_define_module ("nomad app", nomad_app_register_function, NULL);

  /* scm_c_define_module ("nomad minibuffer",
   * nomad_minibuffer_register_function, NULL); */

  scm_c_use_module ("nomad util");
  scm_c_define_module ("nomad util", nomad_util_register_function, NULL);
}
