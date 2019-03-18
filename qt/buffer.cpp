/*
 * webview.c
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
#include <QVariant>
#include <cstddef>
#include <libguile.h>

SCM_DEFINE (scm_nomad_make_buffer, "make-buffer", 0, 1, 0, (SCM uri),
            "Returns a new foreign object buffer for URI. The buffer will "
            "become the current buffer and load URI.")
{
  QVariant arg = QVariant (scm_to_locale_string (uri));

  QMetaObject::invokeMethod (root, "makeBuffer", Qt::BlockingQueuedConnection,
                             Q_ARG (QVariant, arg));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_kill_buffer, "kill-buffer", 0, 0, 0, (),
            "Kill the current buffer")
{
  QMetaObject::invokeMethod (root, "killBuffer", Qt::BlockingQueuedConnection);
  return SCM_UNSPECIFIED;
}

void
buffer_register_functions (void *data)
{
#include "buffer.x"
  scm_c_export ("make-buffer", "kill-buffer", NULL);
}
