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
  GtkBox *box;
  GtkWidget *notebook;
  GtkWidget *pane;
  GtkWidget *read_line;
  GtkWidget *modeline;
  GtkWidget *mini_buffer_label;
  GtkWidget *text_buffer;
  GtkWidget *mini_popup;
  GtkWidget *mini_frame;
  GtkWidget *overlay;
  SCM keymap;
  WebKitWebView *web_view;
};

struct _NomadAppWindow
{
  GtkApplicationWindow parent;
  NomadAppWindowPrivate *priv;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadAppWindow, nomad_app_window,
                            GTK_TYPE_APPLICATION_WINDOW)

// Declarations
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

static void
keyboard_quit (gpointer widget)
{
  NomadAppWindowPrivate *priv;

  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (widget));

  nomad_buffer_grab_view (
      nomad_app_window_get_buffer (NOMAD_APP_WINDOW (widget)));

  /* gtk_widget_hide (priv->mini_popup); */
  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), "");
  scm_call_0 (scm_c_public_ref ("nomad minibuffer", "reset-minibuffer"));
}

void
nomad_app_window_map_event_cb (GtkWidget *widget, gpointer user_data)
{
  keyboard_quit (widget);
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
      printf ("Key press %d %s (unicode %d last_unichar %d)\n", event->keyval,
              event->string, unichar, last_unichar);
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
           */
          flags = emacsy_tick ();
          flags = emacsy_tick ();

          printf ("flags = %d\n", flags);
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

gboolean
window_key_press_cb_old (GtkWidget *widget, GdkEventKey *event)
{
  const gchar *key_name;
  SCM hook;

  hook = scm_c_public_ref ("nomad keymap", "key-press-hook");
  key_name = gdk_keyval_name (event->keyval);

  scm_run_hook (hook,
                scm_list_3 (scm_variable_ref (scm_c_lookup ("global-map")),
                            scm_from_int (event->state),
                            scm_from_locale_string (key_name)));
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
  gtk_paned_set_position (GTK_PANED (widget), (h - (h / 5)));
}

gboolean
minipopup_focus_out_event_cb (GtkWidget *widget, GdkEvent *event,
                              gpointer user_data)
{
  NomadAppWindowPrivate *priv;
  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));
  gtk_widget_hide (widget);
  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), "");
  return FALSE;
}

gboolean
minipopup_focus_in_event_cb (GtkWidget *widget, GdkEvent *event,
                             gpointer user_data)
{
  NomadAppWindowPrivate *priv;
  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));

  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), "C-g");
  gtk_widget_show (widget);
  return FALSE;
}

gboolean
read_line_focus_in_event_cb (GtkWidget *widget, GdkEvent *event,
                             gpointer user_data)
{
  NomadAppWindowPrivate *priv;
  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));

  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), "M-x ");
  clear_read_line_buffer (widget);

  return FALSE;
}

gboolean
read_line_focus_out_event_cb (GtkWidget *widget, GdkEvent *event,
                              gpointer user_data)
{
  NomadAppWindowPrivate *priv;
  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));
  if (gtk_widget_is_focus (priv->mini_popup))
    {
      return TRUE;
    }
  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), "");
  /* gtk_widget_hide (priv->mini_popup); */

  /* g_timeout_add (3500, clear_read_line_buffer, (gpointer)widget); */
  return FALSE;
}

void
read_line_eval (GtkWidget *widget, gpointer user_data)
{
  SCM results;
  SCM msg;
  GtkTextBuffer *buf;
  GtkTextIter start, end;
  gchar *input;

  scm_dynwind_begin (0);
  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);

  input = gtk_text_buffer_get_text (buf, &start, &end, TRUE);

  results = scm_call_1 (scm_c_public_ref ("nomad eval", "input-eval"),
                        scm_from_locale_string (input));

  if (scm_is_string (scm_c_value_ref (results, 1)))
    {
      msg = scm_c_value_ref (results, 1);
    }
  else
    {
      msg = scm_c_value_ref (results, 0);
    }

  gtk_text_buffer_set_text (GTK_TEXT_BUFFER (buf), scm_to_locale_string (msg),
                            -1);

  nomad_buffer_grab_view (
      nomad_app_window_get_buffer (NOMAD_APP_WINDOW (user_data)));

  scm_dynwind_end ();
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

static void
mini_popup_clear (GtkWidget *widget)
{
  GList *children, *iter;
  children = gtk_container_get_children (GTK_CONTAINER (widget));
  for (iter = children; iter != NULL; iter = g_list_next (iter))
    gtk_widget_destroy (GTK_WIDGET (iter->data));
  g_list_free (children);
  scm_c_eval_string ("(set! selected 0)");
}

gboolean
read_line_prompt_press_event_cb (GtkWidget *window, GdkEventKey *event,
                                 gpointer user_data)
{
  SCM result, format, proc;
  NomadAppWindowPrivate *priv;
  GtkTextBuffer *buf;
  GtkTextIter start, end;
  gchar *arg = NULL;

  proc = (SCM)user_data;
  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (window));
  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->read_line));

  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);

  arg = gtk_text_buffer_get_text (buf, &start, &end, TRUE);

  if (event->keyval == GDK_KEY_Return)
    {
      result = scm_call_1 (proc, scm_from_locale_string (arg));
      format = scm_call_3 (scm_c_public_ref ("guile", "format"), SCM_BOOL_F,
                           scm_from_utf8_string ("~a"), result);
      gtk_text_buffer_set_text (buf, scm_to_locale_string (format), -1);

      g_signal_handlers_disconnect_by_func (
          window, read_line_prompt_press_event_cb, user_data);

      // reconnect key release signal
      g_signal_connect (priv->read_line, "key-release-event",
                        G_CALLBACK (read_line_key_release_event_cb),
                        (gpointer)window);

      g_signal_connect (priv->read_line, "key-press-event",
                        G_CALLBACK (read_line_key_press_event_cb),
                        (gpointer)window);

      keyboard_quit (window);
      return TRUE;
    }
  return FALSE;
}

static void
prompt_minibuffer_arg (NomadAppWindow *win, SCM proc, SCM args)
{
  NomadAppWindowPrivate *priv;
  SCM symbol, lst;
  char *prompt;

  priv = nomad_app_window_get_instance_private (win);
  symbol = scm_call_1 (scm_c_private_ref ("ice-9 session", "procedure-name"),
                       proc);
  lst = scm_list_2 (scm_symbol_to_string (symbol),
                    scm_from_utf8_string (" ? "));
  prompt = scm_to_locale_string (scm_string_concatenate (lst));

  // disconnect key release signal
  g_signal_handlers_disconnect_by_func (
      priv->read_line, read_line_key_release_event_cb, (gpointer)win);

  g_signal_handlers_disconnect_by_func (
      priv->read_line, read_line_key_press_event_cb, (gpointer)win);

  // connect prompt signal
  g_signal_connect (GTK_WIDGET (win), "key-press-event",
                    G_CALLBACK (read_line_prompt_press_event_cb),
                    (gpointer)proc);

  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), prompt);
  gtk_widget_grab_focus (priv->read_line);
}

static void
minibuffer_eval_command (GtkWidget *widget, NomadAppWindow *window)
{
  GtkTextBuffer *buf;
  SCM key, proc, args, result, format;

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

  key = scm_call_0 (
      scm_c_public_ref ("nomad minibuffer", "current-selection-ref"));
  proc = scm_call_1 (scm_c_public_ref ("nomad eval", "command-ref"), key);
  args = scm_call_1 (scm_c_public_ref ("nomad eval", "command-args"), key);

  // If command has no arguments call the procedure and display the
  // results in the minibuffer.
  if (scm_is_null (args))
    {
      result = scm_call_0 (proc);
      format = scm_call_3 (scm_c_public_ref ("guile", "format"), SCM_BOOL_F,
                           scm_from_utf8_string ("~a"), result);
      gtk_text_buffer_set_text (buf, scm_to_locale_string (format), -1);
      keyboard_quit (window);
      return;
    }

  // If command has arguments then prompt for argument input.
  prompt_minibuffer_arg (window, proc, args);
}

gboolean
minipopup_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                        gpointer user_data)
{
  SCM scm_hook;
  const gchar *key_name;
  NomadAppWindow *window = NOMAD_APP_WINDOW (user_data);

  scm_hook = scm_c_public_ref ("nomad keymap", "key-press-hook");
  key_name = gdk_keyval_name (event->keyval);

  scm_run_hook (scm_hook,
                scm_list_3 (window->priv->keymap, scm_from_int (event->state),
                            scm_from_locale_string (key_name)));

  /* gtk_widget_grab_focus (GTK_WIDGET (nomad_app_window_get_webview
   * (window))); */
  /* window->priv->keymap = SCM_BOOL_F; */
  return TRUE;
}

gboolean
read_line_key_press_event_cb (GtkWidget *widget, GdkEventKey *event,
                              gpointer user_data)
{
  GdkModifierType modifiers;
  SCM scm_hook;
  const gchar *key_name;

  modifiers = gtk_accelerator_get_default_mod_mask ();
  scm_hook = scm_c_public_ref ("nomad keymap", "key-press-hook");
  key_name = gdk_keyval_name (event->keyval);

  if ((event->state & modifiers) == GDK_CONTROL_MASK)
    {

      scm_run_hook (
          scm_hook,
          scm_list_3 (scm_variable_ref (scm_c_lookup ("minibuffer-mode-map")),
                      scm_from_int (event->state),
                      scm_from_locale_string (key_name)));
      return FALSE;
    }
  return FALSE;
}

gboolean
read_line_key_release_event_cb (GtkWidget *widget, GdkEventKey *event,
                                gpointer user_data)
{
  GdkModifierType modifiers;
  GtkTextBuffer *buf;
  GtkTextIter start, end;
  NomadAppWindowPrivate *priv;
  gchar *input = NULL;
  SCM view, results, select;

  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (user_data));
  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));
  modifiers = gtk_accelerator_get_default_mod_mask ();

  if ((event->state & modifiers) == GDK_CONTROL_MASK)
    {
      return TRUE;
    }

  if (event->keyval == GDK_KEY_Return)
    {
      gtk_widget_hide (priv->mini_popup);
      clear_read_line_buffer (priv->read_line);
      minibuffer_eval_command (widget, NOMAD_APP_WINDOW (user_data));
      return FALSE;
    }

  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);

  input = gtk_text_buffer_get_text (buf, &start, &end, TRUE);

  results
      = scm_call_1 (scm_c_public_ref ("nomad minibuffer", "input-completion"),
                    scm_from_locale_string (input));
  view = scm_c_public_ref ("nomad views", "completion-view");
  select = scm_variable_ref (scm_c_lookup ("current-selection"));

  // If the current selection is greater the size of the results
  // list. Reset the selection to zero
  //
  // FIXME: have the render view check if the selection is in bounds?
  if (scm_to_int (scm_length (results)) < scm_to_int (select))
    {
      select = scm_from_int (0);
      scm_variable_set_x (scm_c_lookup ("current-selection"), select);
    }

  scm_variable_set_x (scm_c_lookup ("current-view"), view);
  scm_variable_set_x (scm_c_lookup ("current-list"), results);

  scm_nomad_minibuffer_render_popup (view, results, select);
  return FALSE;
}

void
minipopup_show_event_cb (GtkWidget *widget, gpointer user_data)
{
  NomadAppWindowPrivate *priv = NOMAD_APP_WINDOW (user_data)->priv;
  gtk_widget_show (priv->mini_frame);
}

void
minipopup_hide_event_cb (GtkWidget *widget, gpointer user_data)
{
  NomadAppWindowPrivate *priv = NOMAD_APP_WINDOW (user_data)->priv;
  gtk_widget_hide (priv->mini_frame);
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

  // signals
  g_signal_connect (G_OBJECT (top), "draw", G_CALLBACK (draw_border_cb), NULL);
  g_signal_connect (G_OBJECT (bottom), "draw", G_CALLBACK (draw_border_cb),
                    NULL);

  g_signal_connect (priv->mini_popup, "key-press-event",
                    G_CALLBACK (minipopup_key_press_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "show",
                    G_CALLBACK (minipopup_show_event_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "hide",
                    G_CALLBACK (minipopup_hide_event_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "focus-out-event",
                    G_CALLBACK (minipopup_focus_out_event_cb), (gpointer)self);

  gtk_widget_hide (overlay_child);
  gtk_widget_show (top);
  gtk_widget_show (bottom);
}

static gboolean
process_and_update_emacsy (void *user_data)
{
  int flags;
  GtkTextBuffer *mbuf, *rbuf;
  NomadAppWindowPrivate *priv;
  NomadAppWindow *self;

  const char *modeline, *status;

  // If user_data is NULL stop this idle process
  if (!user_data)
    {
      return FALSE;
    }

  self = NOMAD_APP_WINDOW (user_data);
  priv = self->priv;

  flags = emacsy_tick ();
  mbuf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->modeline));
  rbuf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->read_line));

  // If there's been a request to quit, quit.
  if (flags & EMACSY_QUIT_APPLICATION_P)
    {
      return FALSE;
    }

  // Update the status line.
  modeline = emacsy_mode_line ();
  status = emacsy_message_or_echo_area ();

  gtk_text_buffer_set_text (mbuf, modeline, -1);

  gtk_text_buffer_set_text (rbuf, status, -1);
  return TRUE;
}

static void
nomad_app_window_init (NomadAppWindow *self)
{
  NomadAppWindowPrivate *priv;
  WebKitCookieManager *cookie_manager;
  char *c_home_page, *c_user_cookie_file;

  gtk_widget_init_template (GTK_WIDGET (self));

  priv = nomad_app_window_get_instance_private (self);
  self->priv = priv;

  scm_dynwind_begin (0);

  priv->keymap = SCM_BOOL_F;

  c_home_page = scm_to_locale_string (
      scm_c_public_ref ("nomad browser", "default-home-page"));

  c_user_cookie_file = scm_to_locale_string (
      scm_c_public_ref ("nomad init", "user-cookie-file"));

  // Connect web extension signals before any new WebViews
  g_signal_connect (webkit_web_context_get_default (),
                    "initialize-web-extensions",
                    G_CALLBACK (initialize_web_extensions), NULL);

  nomad_app_window_overlay_init (self);

  // Minbuf
  gtk_text_view_set_buffer (
      GTK_TEXT_VIEW (priv->read_line),
      GTK_TEXT_BUFFER (text_buffer_new ("classic", "scheme")));

  gtk_text_view_set_buffer (
      GTK_TEXT_VIEW (priv->modeline),
      GTK_TEXT_BUFFER (text_buffer_new ("oblivion", "scheme")));

  // Signals

  // While idle, process events in Emacsy and upate the echo-area.
  g_idle_add ((GSourceFunc)process_and_update_emacsy, self);

  // Main keypress
  g_signal_connect (self, "key-press-event", G_CALLBACK (window_key_press_cb),
                    (gpointer)self);

  /* g_signal_connect (priv->read_line, "focus-out-event",
   *                   G_CALLBACK (read_line_focus_out_event_cb),
   * (gpointer)self); */

  /* g_signal_connect (priv->read_line, "focus-in-event",
   *                   G_CALLBACK (read_line_focus_in_event_cb),
   * (gpointer)self); */

  /* g_signal_connect (priv->read_line, "key-release-event",
   *                   G_CALLBACK (read_line_key_release_event_cb),
   *                   (gpointer)self); */

  /* g_signal_connect (priv->read_line, "key-press-event",
   *                   G_CALLBACK (read_line_key_press_event_cb),
   * (gpointer)self); */

  // Cookies
  cookie_manager = webkit_web_context_get_cookie_manager (
      webkit_web_context_get_default ());

  webkit_cookie_manager_set_persistent_storage (
      cookie_manager, c_user_cookie_file,
      WEBKIT_COOKIE_PERSISTENT_STORAGE_SQLITE);

  gtk_widget_show (priv->read_line);
  // Unwind
  scm_dynwind_free (c_home_page);
  scm_dynwind_free (c_user_cookie_file);
  scm_dynwind_end ();
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

GtkWidget *
nomad_app_window_get_readline (NomadAppWindow *self)
{
  return self->priv->read_line;
}

NomadBuffer *
nomad_app_window_get_buffer (NomadAppWindow *self)
{
  GtkNotebook *notebook = GTK_NOTEBOOK (self->priv->notebook);
  gint i = gtk_notebook_get_current_page (notebook);
  GtkWidget *w = gtk_notebook_get_nth_page (notebook, i);
  return NOMAD_BUFFER (w);
}

void
nomad_app_window_set_keymap (NomadAppWindow *self, SCM keymap)
{
  self->priv->keymap = keymap;
}

void
nomad_app_window_remove_buffer (NomadAppWindow *self)
{
  GtkNotebook *notebook = GTK_NOTEBOOK (self->priv->notebook);
  if (gtk_notebook_get_n_pages (notebook) > 1)
    {
      gtk_notebook_remove_page (notebook,
                                gtk_notebook_get_current_page (notebook));
    }
}

void
nomad_app_window_add_buffer (NomadAppWindow *self, NomadBuffer *buf)
{
  NomadAppWindowPrivate *priv = self->priv;
  gint n = gtk_notebook_append_page (GTK_NOTEBOOK (priv->notebook),
                                     GTK_WIDGET (buf), NULL);
  gtk_widget_show_all (GTK_WIDGET (buf));
  gtk_notebook_set_current_page (GTK_NOTEBOOK (priv->notebook), n);
}

GList *
nomad_window_get_tabs (NomadAppWindow *self)
{
  return gtk_container_get_children (GTK_CONTAINER (self->priv->notebook));
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
  NomadBuffer *buf = nomad_app_window_get_buffer (self);
  return nomad_buffer_get_view (buf);
}

SCM_DEFINE (scm_nomad_window_focus, "webview-focus", 0, 0, 0, (),
            "Switch focus to WebView")
{
  GtkWidget *view = GTK_WIDGET (nomad_app_get_webview (app));
  if (gtk_widget_has_focus (view))
    {
      return SCM_BOOL_F;
    }
  gtk_widget_grab_focus (view);
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_nomad_window_keyboard_quit, "keyboard-quit", 0, 0, 0, (),
            "Quits keyboard input and returns focus to the webview buffer")
{
  NomadAppWindow *win;

  win = NOMAD_APP_WINDOW (nomad_app_get_window (app));

  keyboard_quit (win);
  return SCM_UNDEFINED;
}

SCM_DEFINE (scm_set_web_view_x, "set-web-buffer!", 1, 0, 0,
            (SCM web_buffer_pointer),
            "Set the current web view to the given pointer.")
{
  NomadAppWindow *win = NOMAD_APP_WINDOW (nomad_app_get_window (app));
  GtkWidget *current = nomad_app_get_first_buffer (app);
  GtkNotebook *notebook = nomad_window_get_notebook (win);

  if (SCM_POINTER_P (web_buffer_pointer))
    {
      if (current)
        {
          // If a widget exists mark the widget so it is not destroyed
          // when detaching
          g_object_ref (current);
          gtk_notebook_detach_tab (notebook, current);
        }
      nomad_app_window_add_buffer (win, scm_to_pointer (web_buffer_pointer));
    }
  else
    fprintf (stderr, "error: not given a pointer in set-web-buffer!\n");
  return SCM_UNSPECIFIED;
}

void
nomad_window_register_functions (void *data)
{
#include "window.x"
  scm_c_export ("webview-focus", "keyboard-quit", "set-web-buffer!", NULL);
}
