/*
 * window.c
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

#include <emacsy.h>
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <libguile.h>
#include <libguile/hooks.h>
#include <webkit2/webkit2.h>

#include "app.h"
#include "buffer.h"
#include "minibuffer.h"
#include "util.h"
#include "window.h"

typedef struct _NomadAppWindowPrivate NomadAppWindowPrivate;

struct _NomadAppWindowPrivate
{
  GtkWidget *box;
  GtkWidget *notebook;
  GtkWidget *pane;
  GtkWidget *read_line;
  GtkWidget *modeline;
  GtkWidget *mini_buffer_label;
  GtkWidget *text_buffer;
  GtkWidget *mini_popup;
  GtkWidget *mini_frame;
  GtkWidget *overlay;
};

struct _NomadAppWindow
{
  GtkApplicationWindow parent;
  NomadAppWindowPrivate *priv;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadAppWindow, nomad_app_window,
                            GTK_TYPE_APPLICATION_WINDOW)

// Declarations
static gboolean idle_update_echo_area (void *user_data);

gboolean read_line_key_press_event_cb (GtkWidget *widget, GdkEventKey *event,
                                       gpointer user_data);

gboolean read_line_key_release_event_cb (GtkWidget *widget, GdkEventKey *event,
                                         gpointer user_data);

gboolean read_line_prompt_release_event_cb (GtkWidget *widget,
                                            GdkEventKey *event,
                                            gpointer user_data);

static gboolean
clear_read_line_buffer (gpointer user_data)
{
  GtkTextBuffer *buf;

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (user_data));
  gtk_text_buffer_set_text (buf, "", -1);

  return FALSE;
}

// FIXME: This has been copied verbatim from emacsy example. include
// emacsy copyright
static int
scm_c_char_to_int (const char *char_name)
{
  /* I should put a regex in here to validate it's a char */
  return scm_to_int (scm_char_to_integer (scm_c_eval_string (char_name)));
}

// FIXME: This has been copied verbatim from emacsy example. include
// emacsy copyright
gboolean
window_key_press_cb (GtkWidget *widget, GdkEventKey *event)
{
  static guint32 last_unichar = 0;
  guint32 unichar;
  GdkModifierType modifiers;
  int flags;
  int mod_flags = 0;

  modifiers = gtk_accelerator_get_default_mod_mask ();
  if (event->state & modifiers & GDK_CONTROL_MASK)
    mod_flags |= EMACSY_MODKEY_CONTROL;

  if (event->state & modifiers & GDK_SHIFT_MASK)
    mod_flags |= EMACSY_MODKEY_SHIFT;

  if (event->state & modifiers & GDK_SUPER_MASK)
    mod_flags |= EMACSY_MODKEY_SUPER;

  if (event->state & modifiers & GDK_MOD1_MASK)
    mod_flags |= EMACSY_MODKEY_META;

  unichar = gdk_keyval_to_unicode (event->keyval);

  // Fix up any key values that don't translate perfectly.
  if (event->keyval == GDK_KEY_BackSpace)
    unichar = scm_c_char_to_int ("#\\del");

  // If unichar is 0 then it's not a regular key, e.g., Control, Meta, etc.

  if (event->type == GDK_KEY_PRESS)
    {
      // Fix up some keys.
      if (unichar)
        {
          // Register the key event with Emacsy.
          emacsy_key_event (unichar, mod_flags);
          /*
             One can do the event handling and the actual processing
             separately in Emacsy.  However, in this case, it's convenient
             to do some processing in the event handling here so we know
             whether or not to pass the event on to the browser.

             So we call process_and_update_emacsy to actually do the
             processing.
           */
          idle_update_echo_area (widget);
          flags = emacsy_tick ();

          if (flags & EMACSY_RAN_UNDEFINED_COMMAND_P)
            {
              printf ("Passing to browser.\n");
              return FALSE; // Pass the event through to the web browser.
            }
          else
            {
              printf ("Emacsy handled it.\n");
              last_unichar = unichar;
              return TRUE; // Emacsy handled it. Don't pass the event through.
            }
        }
    }
  else if (event->type == GDK_KEY_RELEASE)
    {
      /*
         We receive both key presses and key releases.  If we decide not
         to pass a key event when pressed, then we remember it
         (last_unichar) such that we squelch the key release event too.
       */
      printf ("Key release %d %s (unicode %d last_unichar %d)\n",
              event->keyval, event->string, unichar, last_unichar);
      if (last_unichar && last_unichar == unichar)
        {
          last_unichar = 0;
          return TRUE; // Don't pass event to the browser.
        }
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

GtkSourceBuffer *
text_buffer_new (const char *theme, const char *lang)
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

gboolean
draw_border_cb (GtkWidget *widget, cairo_t *cr, gpointer data)
{
  guint width, height;
  GdkRGBA color;

  gdk_rgba_parse (&color, "steelblue");

  width = gtk_widget_get_allocated_width (widget);
  height = gtk_widget_get_allocated_height (widget);

  cairo_rectangle (cr, 0, 0, width, height);
  gdk_cairo_set_source_rgba (cr, &color);

  cairo_fill (cr);
  return FALSE;
}

static void
nomad_app_window_overlay_init (NomadAppWindow *self)
{
  GtkWidget *scroll, *overlay_child, *vbox, *top, *bottom;
  NomadAppWindowPrivate *priv = self->priv;

  priv->mini_popup = webkit_web_view_new ();
  priv->mini_frame = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  vbox = priv->mini_frame;

  // borders
  top = gtk_drawing_area_new ();
  bottom = gtk_drawing_area_new ();
  gtk_widget_set_size_request (top, -1, 1);
  gtk_widget_set_size_request (bottom, -1, 1);

  // scroll window
  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_min_content_height (GTK_SCROLLED_WINDOW (scroll),
                                              250);
  gtk_scrolled_window_set_max_content_height (GTK_SCROLLED_WINDOW (scroll),
                                              250);

  // vbox
  overlay_child = vbox;
  gtk_box_pack_start (GTK_BOX (vbox), top, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), priv->mini_popup, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), bottom, FALSE, TRUE, 0);

  gtk_widget_set_size_request (overlay_child, 100, 150);
  gtk_widget_set_halign (overlay_child, GTK_ALIGN_FILL);
  gtk_widget_set_valign (overlay_child, GTK_ALIGN_END);
  gtk_widget_set_margin_bottom (overlay_child, 18);

  gtk_overlay_add_overlay (GTK_OVERLAY (priv->overlay), overlay_child);

  // Notebook
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK (priv->notebook), FALSE);

  // signals
  g_signal_connect (G_OBJECT (top), "draw", G_CALLBACK (draw_border_cb), NULL);
  g_signal_connect (G_OBJECT (bottom), "draw", G_CALLBACK (draw_border_cb),
                    NULL);

  gtk_widget_hide (overlay_child);
  gtk_widget_show (top);
  gtk_widget_show (bottom);
}

static gboolean
idle_update_emacsy (void *user_data)
{
  if (emacsy_tick () & EMACSY_QUIT_APPLICATION_P)
    {
      return FALSE;
    }
  return TRUE;
}

static gboolean
idle_update_modeline (void *user_data)
{
  NomadAppWindow *self;
  GtkTextBuffer *mbuf;
  char *modeline;

  self = NOMAD_APP_WINDOW (user_data);
  mbuf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (self->priv->modeline));

  modeline = emacsy_mode_line ();

  gtk_text_buffer_set_text (mbuf, modeline, -1);

  g_free (modeline);
  return TRUE;
}

static gboolean
idle_update_echo_area (void *user_data)
{
  int flags;
  GtkTextBuffer *rbuf;
  NomadAppWindowPrivate *priv;
  NomadAppWindow *self;
  GtkWidget *webview;
  char *status;

  // If user_data is NULL stop this idle process
  if (!user_data)
    {
      return FALSE;
    }

  self = NOMAD_APP_WINDOW (user_data);
  priv = self->priv;

  flags = emacsy_tick ();
  rbuf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->read_line));

  webview = GTK_WIDGET (nomad_app_window_get_webview (self));

  // If there's been a request to quit, stop this idle process.
  if (flags & EMACSY_QUIT_APPLICATION_P)
    {
      return FALSE;
    }

  // Update the status line.
  status = emacsy_message_or_echo_area ();

  gtk_text_buffer_set_text (rbuf, status, -1);

  g_free (status);

  if (scm_is_true (scm_c_public_ref ("emacsy minibuffer",
                                     "emacsy-display-minibuffer?")))
    {
      gtk_widget_grab_focus (priv->read_line);
      return TRUE;
    }

  if (webview)
    {
      gtk_widget_grab_focus (webview);
    }
  else
    {
      gtk_widget_grab_focus (priv->modeline);
    }
  return TRUE;
}

static void
nomad_app_window_init (NomadAppWindow *self)
{
  NomadAppWindowPrivate *priv = nomad_app_window_get_instance_private (self);
  self->priv = priv;

  // init fields
  priv->overlay = gtk_overlay_new ();
  priv->box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  priv->notebook = gtk_notebook_new ();
  priv->modeline = gtk_source_view_new ();
  priv->read_line = gtk_source_view_new ();

  // tabs
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK (priv->notebook), FALSE);

  // modeline and readline
  gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (priv->modeline), FALSE);

  gtk_text_view_set_buffer (
      GTK_TEXT_VIEW (priv->read_line),
      GTK_TEXT_BUFFER (text_buffer_new ("classic", "scheme")));

  gtk_text_view_set_buffer (
      GTK_TEXT_VIEW (priv->modeline),
      GTK_TEXT_BUFFER (text_buffer_new ("oblivion", "scheme")));

  // add controls
  gtk_container_add (GTK_CONTAINER (self), priv->overlay);
  gtk_container_add (GTK_CONTAINER (priv->overlay), priv->box);

  gtk_box_pack_start (GTK_BOX (priv->box), priv->notebook, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (priv->box), priv->modeline, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (priv->box), priv->read_line, FALSE, FALSE, 0);

  // processes

  // when idle, call emacsy-tick
  g_timeout_add_full (G_PRIORITY_LOW, 50, idle_update_emacsy, NULL, NULL);
  // While idle, update the modeline
  g_timeout_add_full (G_PRIORITY_LOW, 50, idle_update_modeline, self, NULL);

  // signals
  g_signal_connect (self, "key-press-event", G_CALLBACK (window_key_press_cb),
                    (gpointer)self);

  gtk_widget_show_all (GTK_WIDGET (self));
}

GtkNotebook *
nomad_window_get_notebook (NomadAppWindow *self)
{
  return GTK_NOTEBOOK (self->priv->notebook);
}

GtkWidget *
nomad_app_window_get_minipopup (NomadAppWindow *self)
{
  return self->priv->mini_popup;
}

void
nomad_app_window_show_minipopup (NomadAppWindow *self)
{
  gtk_widget_show_all (self->priv->mini_frame);
}

void
nomad_app_window_hide_minipopup (NomadAppWindow *self)
{
  gtk_widget_hide (self->priv->mini_frame);
}

GtkWidget *
nomad_app_window_get_readline (NomadAppWindow *self)
{
  return self->priv->read_line;
}

static void
nomad_app_window_class_init (NomadAppWindowClass *class)
{

  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
                                               "/org/gnu/nomad/window.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, pane);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, read_line);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, modeline);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, notebook);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, overlay);
}

NomadAppWindow *
nomad_app_window_new (NomadApp *app)
{
  return g_object_new (NOMAD_APP_WINDOW_TYPE, "application", app, NULL);
}

WebKitWebView *
nomad_app_window_get_webview (NomadAppWindow *self)
{
  GtkNotebook *notebook = nomad_window_get_notebook (self);
  int page = gtk_notebook_get_current_page (notebook);
  return WEBKIT_WEB_VIEW (gtk_notebook_get_nth_page (notebook, page));
}

SCM_DEFINE (scm_nomad_window_show_tabs, "toggle-tabs", 0, 0, 0, (),
            "Turns notebook tabs on or off")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window ());
  GtkNotebook *notebook = nomad_window_get_notebook (win);

  gtk_notebook_set_show_tabs (notebook,
                              !gtk_notebook_get_show_tabs (notebook));
  return SCM_UNSPECIFIED;
}

void
nomad_window_register_functions (void *data)
{
#include "window.x"
  scm_c_export ("toggle-tabs", NULL);
  scm_c_register_interactive ("toggle-tabs");
}
