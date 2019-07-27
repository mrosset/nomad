/*
 * util.h
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

#ifndef __NOMAD_UTIL_H__
#define __NOMAD_UTIL_H__
#include <libguile.h>

void scm_to_argv (SCM list, char **argv);
SCM scm_c_make_command (const char *key);
SCM scm_c_register_interactive (const char *c_name);
void scm_c_debug_object (SCM object);
SCM scm_c_current_buffer ();
char scm_list_to_c_list (SCM list);

void nomad_util_register_function (void *data);

#endif /* __NOMAD_UTIL_H__ */
