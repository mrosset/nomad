/*
 * text.c
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

#include <gtksourceview/gtksource.h>
#include <libguile.h>

GtkSourceBuffer *
source_buffer_new (const char *theme, const char *lang)
{
  GtkSourceLanguageManager *lm;
  GtkSourceLanguage *sl;
  GtkSourceStyleSchemeManager *sm;
  GtkSourceStyleScheme *ss;
  GtkSourceBuffer *buf;

  buf = gtk_source_buffer_new (NULL);
  sm = gtk_source_style_scheme_manager_new ();
  ss = gtk_source_style_scheme_manager_get_scheme (sm, theme);
  lm = gtk_source_language_manager_new ();
  sl = gtk_source_language_manager_get_language (lm, lang);

  gtk_source_buffer_set_language (buf, sl);
  gtk_source_buffer_set_style_scheme (buf, ss);

  return buf;
}

SCM_DEFINE_PUBLIC (scm_nomad_source_new, "source-new", 0, 0, 0, (),
                   "Returns a newly initialized gtksource view SCM pointer")
{
  GtkWidget *source = gtk_source_view_new_with_buffer (
      source_buffer_new ("classic", "scheme"));

  return scm_from_pointer (source, NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_set_source_text, "set-source-text", 2, 0, 0,
                   (SCM pointer, SCM text),
                   "Sets source view POINTER text to TEXT")
{
  GtkWidget *source = scm_to_pointer (pointer);
  GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (source));
  gtk_text_buffer_set_text (buf, scm_to_locale_string (text), -1);
  return SCM_UNDEFINED;
}

SCM_DEFINE_PUBLIC (scm_nomad_set_point, "set-source-point", 2, 0, 0,
                   (SCM pointer, SCM point),
                   "Sets source view POINTER cursor point to POINT")
{
  /* GtkWidget *source = scm_to_pointer (pointer);
   * GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (source));
   *
   * gtk_text_buffer_place_cursor (buf, const GtkTextIter *where); */
  /* GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (source)); */
  /* gtk_text_buffer_set_text (buf, scm_to_locale_string (text), -1); */
  return SCM_UNDEFINED;
}

void
nomad_text_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "text.x"
#endif
}
