/*
 * scheme.h
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

#ifndef __NOMAD_SCHEME_H__
#define __NOMAD_SCHEME_H__

#include <glib-object.h>
#include <libguile.h>

#define scm_shell_startup_hook                                                \
  scm_c_public_ref ("nomad shell", "%shell-startup-hook")

#define scm_log_debug_ref scm_c_public_ref ("nomad log", "log-debug")

#define scm_log_crit(x)                                                       \
  scm_call_1 (scm_c_public_ref ("nomad log", "log-crit"),                     \
              scm_from_utf8_string (x))

#define scm_log_debug(x)                                                      \
  scm_call_1 (scm_log_debug_ref, scm_from_utf8_string (x))

#define scm_log_func() scm_log_debug (__func__)

G_BEGIN_DECLS

#define NOMAD_TYPE_SCHEME nomad_scheme_get_type ()
G_DECLARE_FINAL_TYPE (NomadScheme, nomad_scheme, NOMAD, SCHEME, GObject)

G_END_DECLS
#endif
