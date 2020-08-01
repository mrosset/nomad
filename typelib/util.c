/*
 * util.c
 * Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>
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

#include "util.h"
#include "../config.h"
#include <libguile.h>

void
nomad_app_run_javascript (WebKitWebView *view, const char *js)
{
  webkit_web_view_run_javascript (view, js, NULL, NULL, NULL);
}

void
nomad_app_set_style (GtkWidget *widget, const char *style)
{
  GtkCssProvider *provider = gtk_css_provider_new ();
  gtk_css_provider_load_from_data (provider, style, -1, NULL);
  gtk_style_context_add_provider (gtk_widget_get_style_context (widget),
                                  GTK_STYLE_PROVIDER (provider),
                                  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);
}

void
nomad_app_send_message (WebKitWebView *view, WebKitUserMessage *message)
{
  webkit_web_view_send_message_to_page (view, message, NULL, NULL, NULL);
}

gboolean
nomad_draw_border (GtkWidget *widget, cairo_t *cr)
{
  guint width, height;
  GdkRGBA color;

  gdk_rgba_parse (&color, "black");

  width = gtk_widget_get_allocated_width (widget);
  height = gtk_widget_get_allocated_height (widget);

  cairo_rectangle (cr, 0, 0, width, height);
  gdk_cairo_set_source_rgba (cr, &color);

  cairo_fill (cr);
  return FALSE;
}

GdkRGBA *
nomad_color_parse (const char *spec)
{
  GdkRGBA color;
  gdk_rgba_parse (&color, spec);
  return gdk_rgba_copy (&color);
}

static GdkRGBA
palette_color (int key)
{
  GdkRGBA color;
  SCM palette = scm_c_public_ref ("nomad terminal", "terminal-palette");
  char *c_string
      = scm_to_locale_string (scm_list_ref (palette, scm_from_int (key)));
  gdk_rgba_parse (&color, c_string);
  return color;
}

void
nomad_vte_set_colors (GtkWidget *widget)
{
  SCM foreground, background;

  foreground = scm_call_0 (
      scm_c_public_ref ("nomad terminal", "terminal-foreground"));
  background = scm_call_0 (
      scm_c_public_ref ("nomad terminal", "terminal-background"));

  vte_terminal_set_colors (
      VTE_TERMINAL (widget),
      nomad_color_parse (scm_to_locale_string (foreground)),
      nomad_color_parse (scm_to_locale_string (background)),
      (const GdkRGBA[]){
          palette_color (0),
          palette_color (1),
          palette_color (2),
          palette_color (3),
          palette_color (4),
          palette_color (5),
          palette_color (6),
          palette_color (7),
          palette_color (8),
          palette_color (9),
          palette_color (10),
          palette_color (11),
          palette_color (12),
          palette_color (13),
          palette_color (14),
          palette_color (15),
      },
      16);
}

static void
scm_nomad_free_argv (void *data)
{
  g_strfreev (data);
}

SCM_DEFINE_PUBLIC (scm_nomad_list_to_argv, "list->argv", 1, 0, 0, (SCM lst),
                   "Converts LST to char **argv and returns SCM pointer")
{
  int len;
  gchar **argv;

  len = scm_to_int (scm_length (lst));
  argv = malloc (sizeof (char *) * len + 1);

  argv[len] = NULL;

  for (int i = 0; i < len; i++)
    {
      SCM item = scm_list_ref (lst, scm_from_int (i));
      argv[i] = scm_to_locale_string (item);
    }

  return scm_from_pointer (argv, scm_nomad_free_argv);
}

void
nomad_set_wrap_mode (GtkTextView *view, gboolean wrap_mode)
{
  gtk_text_view_set_wrap_mode (view, wrap_mode);
}

WebKitNavigationAction *
nomad_get_navigation_action (WebKitPolicyDecision *decision)
{
  return webkit_navigation_policy_decision_get_navigation_action (
      WEBKIT_NAVIGATION_POLICY_DECISION (decision));
}

const char *
nomad_get_version ()
{
  return VERSION;
}
