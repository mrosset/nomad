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

SCM_DEFINE (scm_current_url, "webview-current-url", 0, 0, 0, (),
            "Return's the WebView's current URL.")
{
  QVariant returnedValue;
  QMetaObject::invokeMethod (root, "currentUrl",
                             Q_RETURN_ARG (QVariant, returnedValue));
  qInfo ("%s", returnedValue.toString ().toLatin1 ().data ());
  return scm_from_locale_string (
      returnedValue.toString ().toLatin1 ().data ());
}

void
webview_register_functions (void *data)
{
#include "webview.x"
  scm_c_export ("webview-load-uri", "webview-go-back", "webview-go-forward",
                "webview-reload", "webview-current-url", "scroll-up",
                "scroll-down", NULL);
}
