/*
 * frame.cpp
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

#include <QString>
#include <libguile.h>

SCM_DEFINE (scm_nomad_make_frame, "make-frame", 1, 0, 0, (SCM uri),
            "Creates a new frame loading 'uri")
{
  QString arg = QString (scm_to_utf8_string (uri));
  QMetaObject::invokeMethod (root, "makeFrame", Q_ARG (QVariant, arg));
  return SCM_UNSPECIFIED;
}

void
frame_register_functions (void *data)
{
#include "frame.x"
  scm_c_export ("make-frame", NULL);
}
