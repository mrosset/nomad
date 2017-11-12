/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-  */
/*
 * wemacsappwin.c
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
#include <gtk/gtk.h>

#include "wemacsapp.h"
#include "wemacsappwin.h"

struct _WemacsAppWindow
{
	GtkApplicationWindow parent;
};

typedef struct _WemacsAppWindowPrivate WemacsAppWindowPrivate;

struct _WemacsAppWindowPrivate
{
	GtkWidget *box;
	GtkWidget *vte;
	GtkWidget *webView;
};

G_DEFINE_TYPE_WITH_PRIVATE(WemacsAppWindow, wemacs_app_window, GTK_TYPE_APPLICATION_WINDOW);

static void
wemacs_app_window_init (WemacsAppWindow *win)
{
	gtk_widget_init_template (GTK_WIDGET (win));
}

static void
wemacs_app_window_class_init (WemacsAppWindowClass *class)
{
	gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
												 "/org/gnu/wemacseapp/window.ui");
	gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class), WemacsAppWindow, box);

}

WemacsAppWindow *
wemacs_app_window_new (WemacsApp *app)
{
	return g_object_new (WEMACS_APP_WINDOW_TYPE, "application", app, NULL);
}
