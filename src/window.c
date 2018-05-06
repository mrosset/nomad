/*
 * nomadappwin.c
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 *
 * nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * nomad is distributed in the hope that it will be useful, but
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

struct _NomadAppWindow
{
  GtkApplicationWindow parent;
};

typedef struct _NomadAppWindowPrivate NomadAppWindowPrivate;

struct _NomadAppWindowPrivate
{
  GtkBox *box;
  GtkWidget *minibuf;
  GtkWidget *page_url;
  GtkWidget *pane;
  GtkWidget *read_line;
  GtkWidget *result_popover;
  GtkWidget *result_popover_view;
  GtkWidget *vte;
  WebKitWebView *web_view;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadAppWindow, nomad_app_window,
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
  NomadAppWindowPrivate *priv;
  GtkWidget *vte;
  SCM scm_hook;

  const gchar *key_name;

  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (widget));
  vte = priv->vte;
  modifiers = gtk_accelerator_get_default_mod_mask ();
  key_name = gdk_keyval_name (event->keyval);

  // Handles M-m
  if (event->keyval == GDK_KEY_m
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      if (!gtk_widget_is_visible (vte))
        {
          gtk_widget_hide (priv->read_line);
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
          gtk_widget_show (priv->read_line);
          gtk_widget_hide (vte);
        }
      return TRUE;
    }

  // Handles M-x
  if (event->keyval == GDK_KEY_x
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      if (!gtk_widget_has_focus (priv->read_line)
          && !gtk_widget_has_focus (priv->vte))
        {
          gtk_widget_grab_focus (priv->read_line);
        }
      return FALSE;
    }

  // If the webview does note have focus return FALSE, so children can
  // handle key events
  if (!gtk_widget_has_focus (GTK_WIDGET (priv->web_view)))
    {
      return FALSE;
    }

  // If event has state then its a modified keypress eg. `C-c' but not
  // `C-c c' which means we can't handle prefixes, quite yet.  since
  // we can easily capture this state, we'll use this a starting point
  // for our keybindings. We'll call our Scheme key-press-hook. from
  // here the nomad keymap module will do the work.
  if ((event->state & modifiers) == GDK_CONTROL_MASK)
    {
      scm_hook = scm_c_public_ref ("nomad keymap", "key-press-hook");
      scm_run_hook (scm_hook, scm_list_2 (scm_from_int (event->state),
                                          scm_from_locale_string (key_name)));
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
                                                   NOMAD_WEB_EXTENSIONS_DIR);
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
clear_read_line_buffer (gpointer user_data)
{
  GtkTextBuffer *buf;

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (user_data));
  gtk_text_buffer_set_text (buf, "", -1);

  return FALSE;
}

gboolean
delayed_popdown (gpointer user_data)
{
  gtk_widget_grab_focus (GTK_WIDGET (user_data));

  return FALSE;
}

gboolean
read_line_focus_in_event_cb (GtkWidget *widget, GdkEvent *event,
                             gpointer user_data)
{
  clear_read_line_buffer (widget);

  return FALSE;
}

gboolean
read_line_focus_out_event_cb (GtkWidget *widget, GdkEvent *event,
                              gpointer user_data)
{
  // g_timeout_add (3500, clear_read_line_buffer, (gpointer)widget);
  return FALSE;
}

void
read_line_eval (GtkWidget *widget, gpointer user_data)
{

  SCM proc;
  SCM value;
  gchar *result;
  GtkTextBuffer *buf;
  GtkTextIter start, end;
  gchar *input;
  NomadAppWindowPrivate *priv;

  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));
  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);

  input = gtk_text_buffer_get_text (buf, &start, &end, TRUE);

  proc = scm_c_public_ref ("nomad util", "catch-eval");
  value = scm_call_1 (proc, scm_take_locale_string (input));

  if (!scm_is_string (value))
    {
      value = scm_from_utf8_string ("unhandled value: not a string");
      g_critical ("unhandled value: not a string");
    }

  result = scm_to_locale_string (value);

  gtk_text_buffer_set_text (GTK_TEXT_BUFFER (buf), result, -1);

  gtk_widget_grab_focus (GTK_WIDGET (priv->web_view));

  g_free (result);
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
minibuf_key_press_cb (GtkWidget *view, GdkEventKey *event, gpointer user_data)
{
  if (event->keyval == GDK_KEY_Return)
    {
      read_line_eval (view, user_data);
      return TRUE;
    }

  return FALSE;
}

static void
nomad_app_window_init (NomadAppWindow *win)
{

  NomadAppWindowPrivate *priv;
  WebKitCookieManager *cookie_manager;
  char *c_home_page;
  char *c_user_cookie_file;
  SCM home_page;

  gtk_widget_init_template (GTK_WIDGET (win));

  scm_dynwind_begin (0);

  priv = nomad_app_window_get_instance_private (win);
  home_page = scm_c_public_ref ("nomad browser", "default-home-page");
  c_home_page = scm_to_locale_string (home_page);
  c_user_cookie_file = scm_to_locale_string (
      scm_c_public_ref ("nomad init", "user-cookie-file"));

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
                    G_CALLBACK (minibuf_key_press_cb), (gpointer)win);
  // Vte
  priv->vte = GTK_WIDGET (nomad_vte_new ());

  gtk_text_view_set_buffer (GTK_TEXT_VIEW (priv->result_popover_view),
                            GTK_TEXT_BUFFER (minibuf_new ()));
  // Packing
  gtk_box_pack_start (GTK_BOX (priv->box), GTK_WIDGET (priv->web_view), TRUE,
                      TRUE, 0);
  gtk_paned_add2 (GTK_PANED (priv->pane), GTK_WIDGET (priv->vte));
  gtk_widget_show_all (priv->pane);
  gtk_widget_hide (priv->vte);
  webkit_web_view_load_uri (priv->web_view, c_home_page);

  // Cookies
  cookie_manager = webkit_web_context_get_cookie_manager (
      webkit_web_context_get_default ());

  webkit_cookie_manager_set_persistent_storage (
      cookie_manager, c_user_cookie_file,
      WEBKIT_COOKIE_PERSISTENT_STORAGE_SQLITE);
  scm_dynwind_free (c_user_cookie_file);
  scm_dynwind_free (c_home_page);
  scm_dynwind_end ();
}

static void
nomad_app_window_class_init (NomadAppWindowClass *class)
{

  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
                                               "/org/gnu/nomadeapp/window.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, pane);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, page_url);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, read_line);
  gtk_widget_class_bind_template_child_private (
      GTK_WIDGET_CLASS (class), NomadAppWindow, result_popover);
  gtk_widget_class_bind_template_child_private (
      GTK_WIDGET_CLASS (class), NomadAppWindow, result_popover_view);
}

NomadAppWindow *
nomad_app_window_new (NomadApp *app)
{
  return g_object_new (NOMAD_APP_WINDOW_TYPE, "application", app, NULL);
}

WebKitWebView *
nomad_app_window_get_webview (NomadAppWindow *win)
{
  NomadAppWindowPrivate *priv;

  priv = nomad_app_window_get_instance_private (win);

  return priv->web_view;
}
