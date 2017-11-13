/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-  */
/*
 * scm.c
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
#include <libguile.h>
#include <gtk/gtk.h>

#include "app.h"

#define WEMACS_VERSION "0.1"

SCM_DEFINE (scm_wemacs_version, "wemacs-version", 0, 0, 0, (),
			"test macro")
{
	return scm_from_locale_string (WEMACS_VERSION);
}

SCM_DEFINE (scm_wemacs_start, "wemacs-start", 0, 0, 0, (),
			"test macro")
{
	intmax_t status;
	GApplication * app;
	app = G_APPLICATION(wemacs_app_new());
	status = g_application_run (app, 0, NULL);
	return  scm_from_intmax(status);
}

void *
register_functions (void *data)
{
#include "scheme.x"
  /* scm_c_define_gsubr ("wemacs-version", 0, 0, 0, &wemacs_version); */
  /* scm_c_define_gsubr ("wemacs-start", 0, 0, 0, &wemacs_start); */
  /* scm_c_define_gsubr ("wemacs-quit", 0, 0, 0, &wemacs_quit); */
  /* scm_c_define_gsubr ("close-emacs", 0, 0, 0, &close_emacs); */
  /* scm_c_define_gsubr ("web-view-load-uri", 1, 0, 0, &web_view_load_uri); */
  /* scm_c_define_gsubr ("web-view-go-back", 0, 0, 0, &web_view_go_back); */
  /* scm_c_define_gsubr ("web-view-go-forward", 0, 0, 0, &web_view_go_forward); */
  /* scm_c_define_gsubr ("web-view-reload", 0, 0, 0, &web_view_reload); */
}
