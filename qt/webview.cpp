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

SCM_DEFINE (scm_webview_current_url, "webview-current-url", 0, 0, 0, (),
            "Return's the WebView's current URL.")
{
  QVariant value = invoke_method (root, "currentUrl");
  char *url = value.toString ().toLatin1 ().data ();
  qInfo ("qml: %s", url);
  return scm_from_locale_string (url);
}

SCM_DEFINE (scm_webview_load_uri, "webview-load-uri", 1, 0, 0, (SCM uri),
            "Set's the current WebView to uri")
{
  QVariant arg = QVariant (scm_to_locale_string (uri));

  QMetaObject::invokeMethod (root, "setUrl", Qt::BlockingQueuedConnection,
                             Q_ARG (QVariant, arg));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (
    scm_webview_go_back, "webview-go-back", 0, 0, 0, (),
    "Request WebView to go back in history. If WebView can not be found or "
    "there is no back history then it return #f. Otherwise it returns #t.")
{
  QMetaObject::invokeMethod (root, "goBack", Qt::BlockingQueuedConnection);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (
    scm_nomad_webkit_go_foward, "webview-go-forward", 0, 0, 0, (),
    "Internal request WebKitView to go forward in history. If WebView can not "
    "be found or there is no forward history then it returns #f. Otherwise it "
    "returns #t. TODO: maybe provide a callback for load-change signal.")
{
  QMetaObject::invokeMethod (root, "goForward", Qt::BlockingQueuedConnection);
  return SCM_UNSPECIFIED;
}

void
webview_register_functions (void *data)
{
#include "webview.x"
  scm_c_export ("webview-load-uri", "webview-go-back", "webview-go-forward",
                "webview-reload", "webview-current-url", "scroll-up",
                "scroll-down", NULL);
}
