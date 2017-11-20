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
#include <gtksourceview/gtksource.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <libguile.h>
#include <webkit2/webkit2.h>

#include "app.h"
#include "vte.h"
#include "window.h"

struct _WemacsAppWindow
{
  GtkApplicationWindow parent;
};

typedef struct _WemacsAppWindowPrivate WemacsAppWindowPrivate;

struct _WemacsAppWindowPrivate
{
  GtkBox *box;
  GtkWidget *minibuf;
  GtkWidget *read_line;
  GtkWidget *page_url;
  GtkWidget *pane;
  GtkWidget *vte;
  WebKitWebView *web_view;
};

G_DEFINE_TYPE_WITH_PRIVATE (WemacsAppWindow, wemacs_app_window,
                            GTK_TYPE_APPLICATION_WINDOW)

static void
web_view_load_changed (WebKitWebView *web_view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  GtkLabel *label;
  const gchar *uri;

  uri = webkit_web_view_get_uri (web_view);
  label = GTK_LABEL (user_data);
  gtk_label_set_text (label, uri);
}

gboolean
key_press_cb (GtkWidget *widget, GdkEventKey *event)
{
  GdkModifierType modifiers;
  WemacsAppWindowPrivate *priv;
  GtkWidget *vte;

  priv = wemacs_app_window_get_instance_private (WEMACS_APP_WINDOW (widget));
  vte = priv->vte;
  modifiers = gtk_accelerator_get_default_mod_mask ();

  // Handles M-m
  if (event->keyval == GDK_KEY_m
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      if (!gtk_widget_is_visible (vte))
        {
          gtk_widget_show (vte);
          gtk_widget_grab_focus (vte);
          return TRUE;
        }
      if (!gtk_widget_has_focus (vte))
        {
          gtk_widget_grab_focus (vte);
          return TRUE;
        }
      if (gtk_widget_is_visible (vte) && gtk_widget_has_focus (vte))
        {
          gtk_widget_hide (vte);
        }
      return TRUE;
    }

  // Handles M-x
  if (event->keyval == GDK_KEY_x
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      if (!gtk_widget_has_focus (priv->read_line))
        {
          gtk_widget_grab_focus (priv->read_line);
        }
      else
        {
          gtk_widget_grab_focus (GTK_WIDGET (priv->web_view));
        }
      return TRUE;
    }

  return FALSE;
}

static void
initialize_web_extensions (WebKitWebContext *context, gpointer user_data)
{
  /* Web Extensions get a different ID for each Web Process */
  static guint32 unique_id = 1;

  webkit_web_context_set_web_extensions_directory (context,
                                                   WEMACS_WEB_EXTENSIONS_DIR);
  webkit_web_context_set_web_extensions_initialization_user_data (
      context, g_variant_new_uint32 (unique_id++));
}

void
pane_size_allocate_cb (GtkWidget *widget, gpointer data)
{
  gint h = gtk_widget_get_allocated_height (widget);
  gtk_paned_set_position (GTK_PANED (widget), (h - (h / 4)));
}

gboolean
clear_read_line_buffer (gpointer widget)
{
  GtkTextBuffer *buf;
  if (!gtk_widget_has_focus (GTK_WIDGET (widget)))
    {
      buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));
      gtk_text_buffer_set_text (buf, "", -1);
    }
  return FALSE;
}

gboolean
read_line_focus_in_event_cb (GtkWidget *widget, GdkEvent *event,
                             gpointer user_data)
{
  if (!gtk_widget_has_focus (widget))
    {
      clear_read_line_buffer (widget);
    }
  return FALSE;
}

gboolean
read_line_focus_out_event_cb (GtkWidget *widget, GdkEvent *event,
                              gpointer user_data)
{
  g_timeout_add (5000, clear_read_line_buffer, widget);
  return FALSE;
}

void
read_line_eval (GtkWidget *widget, gpointer user_data)
{

  SCM exp;
  SCM value;
  gchar *result;
  GtkTextBuffer *buf;
  GtkTextIter start, end;
  gchar *input;

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);

  input = gtk_text_buffer_get_text (buf, &start, &end, TRUE);

  exp = scm_list_2 (scm_from_locale_symbol ("quasi-eval"),
                    scm_from_locale_string (input));

  value = scm_eval (exp, scm_interaction_environment ());

  if (!scm_is_string (value))
    {
      value = scm_from_locale_string ("unhandled value: not a string");
      g_critical ("unhandled value: not a string");
    }

  result = scm_to_locale_string (value);
  g_print ("result: %s", result);

  gtk_text_buffer_set_text (buf, result, -1);
  gtk_widget_grab_focus (GTK_WIDGET (user_data));
  g_free (result);
}

gboolean
source_view_focus_out_cb (GtkWidget *widget, gpointer user_data)

{
  gtk_widget_hide (widget);
  return FALSE;
}

void
source_view_focus_in_cb (GtkWidget *widget, gpointer user_data)

{
  gtk_widget_show (widget);
}

GtkSourceBuffer *
minibuf_new ()
{

  GtkSourceLanguageManager *lm;
  GtkSourceLanguage *sl;
  GtkSourceStyleSchemeManager *sm;
  GtkSourceStyleScheme *ss;

  GtkSourceBuffer *buf;

  buf = gtk_source_buffer_new (NULL);
  sm = gtk_source_style_scheme_manager_new ();
  ss = gtk_source_style_scheme_manager_get_scheme (sm, "solarized-light");
  lm = gtk_source_language_manager_new ();
  sl = gtk_source_language_manager_get_language (lm, "scheme");

  gtk_source_buffer_set_language (buf, sl);
  gtk_source_buffer_set_style_scheme (buf, ss);

  return buf;
}

gboolean
minibuf_key_press_cb (GtkWidget *view, GdkEventKey *event, gpointer data)
{
  if (event->keyval == GDK_KEY_Return)
    {
      read_line_eval (view, data);
      return TRUE;
    }

  return FALSE;
}

static void
wemacs_app_window_init (WemacsAppWindow *win)
{

  WemacsAppWindowPrivate *priv;

  gtk_widget_init_template (GTK_WIDGET (win));

  priv = wemacs_app_window_get_instance_private (win);

  g_signal_connect (webkit_web_context_get_default (),
                    "initialize-web-extensions",
                    G_CALLBACK (initialize_web_extensions), NULL);

  // WebView
  priv->web_view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  g_signal_connect (priv->web_view, "load-changed",
                    G_CALLBACK (web_view_load_changed), priv->page_url);
  // Minbuf
  priv->minibuf = GTK_WIDGET (minibuf_new ());
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (priv->read_line),
                            GTK_TEXT_BUFFER (priv->minibuf));

  g_signal_connect (priv->read_line, "key-press-event",
                    G_CALLBACK (minibuf_key_press_cb),
                    (gpointer)priv->web_view);
  // Vte
  priv->vte = GTK_WIDGET (wemacs_vte_new ());

  // Packing
  gtk_box_pack_start (GTK_BOX (priv->box), GTK_WIDGET (priv->web_view), TRUE,
                      TRUE, 0);
  gtk_paned_add2 (GTK_PANED (priv->pane), GTK_WIDGET (priv->vte));
  gtk_widget_show_all (priv->pane);
  gtk_widget_hide (priv->vte);
  webkit_web_view_load_uri (priv->web_view, DEFAULT_URI);
}

static void
wemacs_app_window_class_init (WemacsAppWindowClass *class)
{

  gtk_widget_class_set_template_from_resource (
      GTK_WIDGET_CLASS (class), "/org/gnu/wemacseapp/window.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                WemacsAppWindow, pane);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                WemacsAppWindow, page_url);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                WemacsAppWindow, box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                WemacsAppWindow, read_line);
}

WemacsAppWindow *
wemacs_app_window_new (WemacsApp *app)
{
  return g_object_new (WEMACS_APP_WINDOW_TYPE, "application", app, NULL);
}

WebKitWebView *
wemacs_app_window_get_webview (WemacsAppWindow *win)
{
  WemacsAppWindowPrivate *priv;

  priv = wemacs_app_window_get_instance_private (win);

  return priv->web_view;
}
