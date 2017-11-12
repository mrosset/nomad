/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-  */
/*
 * wemacsappwin.h
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 * 
 * wemacs is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * wemacs is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __WEMACSAPPWIN_H
#define __WEMACSAPPWIN_H

#include <gtk/gtk.h>
#include "wemacsapp.h"


#define WEMACS_APP_WINDOW_TYPE (wemacs_app_window_get_type ())
G_DECLARE_FINAL_TYPE (WemacsAppWindow, wemacs_app_window, WEMACS, APP_WINDOW, GtkApplicationWindow)

WemacsAppWindow       *wemacs_app_window_new          (WemacsApp *app);

#endif /* __WEMACSAPPWIN_H */
