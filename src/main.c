/*
 * nomad-shell.c
 * Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>
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

#include <libguile.h>

static void
inner_main (void *data, int argc, char **argv)
{
  /* scm_c_use_module ("nomad gtk gi"); */
  scm_c_use_module ("nomad nomad");
  /* scm_c_use_module ("nomad platform"); */

  // We need to call init for so things like GDK_SCALE are used by our
  // GApplication
  /* scm_call_0 (scm_c_public_ref ("nomad init", "init")); */
  /* scm_call_0 (scm_c_public_ref ("nomad gtk gtk", "main")); */
  /* nomad_start_mutter (); */
}

int
main (int argc, char *argv[])
{

  scm_boot_guile (argc, argv, inner_main, NULL);
  return 0;
}
