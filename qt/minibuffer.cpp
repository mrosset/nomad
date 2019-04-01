/*
 * minibuffer.c
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
#include <QQmlProperty>
#include <cstddef>
#include <libguile.h>

SCM_DEFINE (scm_minibuffer_scroll_down, "minibuffer-scroll-down", 0, 0, 0, (),
            "Set's the current WebView to URI")
{
  QObject *mb = window->findChild<QObject *> ("miniOutput");
  QMetaObject::invokeMethod (mb, "selectDown", Qt::DirectConnection);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_minibuffer_scroll_up, "minibuffer-scroll-up", 0, 0, 0, (),
            "Set's the current WebView to URI")
{
  QObject *mb = window->findChild<QObject *> ("miniOutput");
  QMetaObject::invokeMethod (mb, "selectUp", Qt::DirectConnection);
  return SCM_UNDEFINED;
}

void
minibuffer_register_functions (void *data)
{

#include "minibuffer.x"
  scm_c_export ("minibuffer-scroll-down", "minibuffer-scroll-up", NULL);
}
