/*
 * scheme.c
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

#include "scheme.h"

typedef struct _NomadSchemePrivate NomadSchemePrivate;

struct _NomadSchemePrivate
{
  /* SCM buffer; */
  const char *field;
};

struct _NomadScheme
{
  GObject parent;
  NomadSchemePrivate *priv;
};

// clang-format off
G_DEFINE_TYPE_WITH_PRIVATE (NomadScheme, nomad_scheme, G_TYPE_OBJECT);
// clang-format on

static void
nomad_scheme_init (NomadScheme *self)
{
  self->priv = nomad_scheme_get_instance_private (self);
}

static void
nomad_scheme_class_init (NomadSchemeClass *class)
{
}
