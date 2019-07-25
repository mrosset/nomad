/*
 * webkit.c
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

#include <libguile.h>
#include <webkit2/webkit2.h>

SCM_DEFINE_PUBLIC (scm_nomad_webkit_new, "webkit-new", 0, 0, 0, (SCM pointer),
                   "Returns a newly initialized webkit view")
{
  GtkWidget *view = webkit_web_view_new ();
  return scm_from_pointer (view, NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_uri, "webkit-uri", 1, 0, 0, (SCM pointer),
                   "Returns the current uri for a webkit view pointer. If "
                   "webview has not uri it returns #f")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  const char *uri = webkit_web_view_get_uri (WEBKIT_WEB_VIEW (view));
  if (uri)
    {
      return scm_from_locale_string (uri);
    }
  return scm_from_utf8_string ("NULL");
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_load_uri, "webkit-load-uri", 2, 0, 0,
                   (SCM pointer, SCM uri),
                   "Requests webkit VIEW pointer to load URI")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  char *c_uri = scm_to_locale_string (uri);
  webkit_web_view_load_uri (WEBKIT_WEB_VIEW (view), c_uri);
  g_free (c_uri);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_load_html, "webkit-load-html", 2, 0, 0,
                   (SCM pointer, SCM html),
                   "Requests webkit VIEW pointer to load HTML")
{
  GtkWidget *view = (GtkWidget *)scm_to_pointer (pointer);
  char *c_content = scm_to_locale_string (html);
  webkit_web_view_load_html (WEBKIT_WEB_VIEW (view), c_content, "nomad://");
  g_free (c_content);
  return SCM_UNSPECIFIED;
}

void
nomad_webkit_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkit.x"
#endif
}
