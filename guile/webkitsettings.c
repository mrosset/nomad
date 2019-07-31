/*
 * webkit-settings --- configure webkit renderer.
 *
 * Copyright (C) 2019 Amar Singh<nly@disroot.org>
 *
 * This file is part of Nomad.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "webkit.h"
#include "util.h"
#include "app.h"
#include <libguile.h>
#include <webkit2/webkit2.h>


SCM_DEFINE_PUBLIC (scm_nomad_say_meow, "say-meow", 0, 0, 0, (),
                   "Test if this module is working.")
{
  return SCM_BOOL_T;
}

void
nomad_webkitsettings_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkitsettings.x"
#endif
}
