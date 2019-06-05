/*
 * qml.cpp
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

#include <QObject>
#include <libguile.h>

SCM_DEFINE (scm_qml_set_bool, "qml-set-bool", 3, 0, 0,
            (SCM name, SCM property, SCM value), "Set objects bool property")
{

  QObject *object = window->findChild<QObject *> (scm_to_qstring (name));

  bool qvalue = false;

  if (!object)
    {
      return SCM_BOOL_F;
    }

  if (scm_is_true (value))
    {
      qvalue = true;
    }

  object->setProperty (scm_to_utf8_string (property), qvalue);
  return SCM_UNSPECIFIED;
}

void
qml_register_functions (void *data)
{
#include "qml.x"
  scm_c_export ("qml-set-bool", NULL);
}
