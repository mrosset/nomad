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
#include "keymap.h"

#include <QApplication>
#include <QClipboard>
#include <QQmlProperty>
#include <QVariant>
#include <cstddef>
#include <libguile.h>

QObject *
currentWebView ()
{
  return qvariant_cast<QObject *> (
      QQmlProperty::read (root, "currentWebView"));
}

SCM_DEFINE (scm_webview_current_url, "webview-current-url", 0, 0, 0, (),
            "Return's the Web View's current URL.")
{
  QVariant value = qvariant_cast<QVariant> (
      QQmlProperty::read (currentWebView (), "url"));
  const char *url = value.toString ().toLatin1 ().data ();
  return scm_from_utf8_string (url);
}

SCM_DEFINE (scm_webview_load_uri, "webview-load-uri", 1, 0, 0, (SCM uri),
            "Set's the current WebView to URI")
{
  QVariant arg = QVariant (scm_to_qstring (uri));

  keymap.SetUrl (arg);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (
    scm_webview_go_back, "webview-go-back", 0, 0, 0, (),
    "Request WebView to go back in history. If WebView can not be found or "
    "there is no back history then it return #f. Otherwise it returns #t.")
{
  keymap.handleGoBack ();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (
    scm_nomad_webkit_go_foward, "webview-go-forward", 0, 0, 0, (),
    "Internal request WebView to go forward in history. If WebView can not "
    "be found or there is no forward history then it returns #f. Otherwise it "
    "returns #t. TODO: maybe provide a callback for load-change signal.")
{
  keymap.handleGoForward ();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_webview_reload, "webview-reload", 0, 0, 0, (),
            "Reloads the current buffer")
{
  QMetaObject::invokeMethod (currentWebView (), "reload",
                             Qt::BlockingQueuedConnection);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_nomad_scroll_up, "scroll-up", 0, 0, 0, (),
            "Scrolls the current webview up")
{
  keymap.scrollv (-100);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_scroll_down, "scroll-down", 0, 0, 0, (),
            "Scrolls the current webview down")
{
  keymap.scrollv (100);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_debug_webview, "debug-webview-methods", 0, 0, 0, (),
            "prints webview methods")
{
  print_methods (window);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_nomad_copy_current_url, "copy-current-url", 0, 0, 0, (),
            "prints webview methods")
{
  QClipboard *cb = QApplication::clipboard ();
  const QVariant uri = qvariant_cast<QVariant> (
      QQmlProperty::read (currentWebView (), "url"));

  cb->setText (uri.toString (), QClipboard::Clipboard);
  return scm_from_utf8_string (uri.toString ().toUtf8 ().constData ());
}

SCM_DEFINE (scm_nomad_find, "isearch-forward", 1, 0, 0, (SCM key),
            "search forward")
{
  QString arg = QString (scm_to_utf8_string (key));
  QMetaObject::invokeMethod (currentWebView (), "findText",
                             Qt::BlockingQueuedConnection,
                             Q_ARG (QString, arg));
  return SCM_UNDEFINED;
}

void
webview_register_functions (void *data)
{
#include "webview.x"
  scm_c_export ("webview-load-uri", "webview-go-back", "webview-go-forward",
                "webview-reload", "webview-current-url", "scroll-up",
                "scroll-down", "debug-webview-methods", "copy-current-url",
                "isearch-forward", NULL);
}
