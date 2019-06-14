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

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <libguile.h>
#include <libguile/hooks.h>
#include <vte/vte.h>
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
  GtkWidget *mini_buffer_label;
  GtkWidget *text_buffer;
  GtkWidget *mini_popup;
  GtkWidget *mini_frame;
  GtkWidget *vte;
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

static void
fork_vte_child (VteTerminal *vte, gint status, gpointer data)
{
  gchar **envv;
  gchar *pwd;
  SCM cmd = scm_c_public_ref ("nomad repl", "repl-command-line");
  gchar *argv[scm_to_int (scm_length (cmd)) + 1];

  scm_to_argv (cmd, argv);
  pwd = g_get_current_dir ();
  envv = g_get_environ ();
  envv = g_environ_setenv (envv, "TERM", "xterm-256color", TRUE);

  vte_terminal_spawn_async (vte, VTE_PTY_DEFAULT, NULL, argv, envv,
                            G_SPAWN_DEFAULT | G_SPAWN_SEARCH_PATH_FROM_ENVP,
                            NULL, 0, NULL, -1, NULL, NULL, NULL);

  g_strfreev (envv);
  g_free (pwd);
  gtk_widget_grab_focus (GTK_WIDGET (vte));
}

void
nomad_app_window_map_event_cb (GtkWidget *widget, gpointer user_data)
{
  keyboard_quit (widget);
}

gboolean
window_key_press_cb (GtkWidget *widget, GdkEventKey *event)
{
  GdkModifierType modifiers;
  NomadAppWindowPrivate *priv;
  GtkWidget *vte;
  SCM view;

  priv = nomad_app_window_get_instance_private (NOMAD_APP_WINDOW (widget));
  vte = priv->vte;
  modifiers = gtk_accelerator_get_default_mod_mask ();

  // When read line does not have focus. Clear it's input
  if (!gtk_widget_has_focus (priv->read_line))
    {
      clear_read_line_buffer (priv->read_line);
    }

  // If the vte has focus return FALSE, so children can handle key
  // events
  if (gtk_widget_has_focus (GTK_WIDGET (priv->vte)))
    {
      return FALSE;
    }

  if ((event->state & modifiers) == GDK_CONTROL_MASK
      && event->keyval == GDK_KEY_x)
    {
      view = scm_c_public_ref ("nomad views", "which-key-view");
      priv->keymap = scm_variable_ref (scm_c_lookup ("ctl-x-map"));
      scm_nomad_minibuffer_render_popup (view, priv->keymap, scm_from_int (0));
      gtk_widget_grab_focus (priv->mini_popup);
      return TRUE;
    }

  // Handles M-m
  if (event->keyval == GDK_KEY_m
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      if (!gtk_widget_is_visible (vte) || !gtk_widget_has_focus (vte))
        {
          gtk_widget_hide (priv->read_line);
          gtk_widget_show (vte);
          gtk_widget_grab_focus (vte);
          return TRUE;
        }
      if (gtk_widget_is_visible (vte) && gtk_widget_has_focus (vte))
        {
          gtk_widget_show (priv->read_line);
          gtk_widget_hide (vte);
          nomad_buffer_grab_view (
              nomad_app_window_get_buffer (NOMAD_APP_WINDOW (widget)));
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
          gtk_widget_show (priv->mini_popup);
        }

      return TRUE;
    }

  // If C-g then do keyboard quit
  if ((event->state & modifiers) == GDK_CONTROL_MASK
      && event->keyval == GDK_KEY_g)
    {
      keyboard_quit (widget);
      gtk_widget_hide (priv->mini_popup);
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
text_buffer_new ()
{
  GtkSourceLanguageManager *lm;
  GtkSourceLanguage *sl;
  /* GtkSourceStyleSchemeManager *sm; */
  /* GtkSourceStyleScheme *ss; */
  GtkSourceBuffer *buf;

  buf = gtk_source_buffer_new (NULL);
  /* sm = gtk_source_style_scheme_manager_new (); */
  /* ss = gtk_source_style_scheme_manager_get_scheme (sm, "classic"); */
  lm = gtk_source_language_manager_new ();
  sl = gtk_source_language_manager_get_language (lm, "scheme");

  gtk_source_buffer_set_language (buf, sl);
  /* gtk_source_buffer_set_style_scheme (buf, ss); */
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
read_line_prompt_release_event_cb (GtkWidget *window, GdkEventKey *event,
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
          window, read_line_prompt_release_event_cb, user_data);

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
  g_signal_connect (GTK_WIDGET (win), "key-release-event",
                    G_CALLBACK (read_line_prompt_release_event_cb),
                    (gpointer)proc);

  gtk_label_set_text (GTK_LABEL (priv->mini_buffer_label), prompt);
  gtk_widget_grab_focus (priv->read_line);
}

static void
minibuffer_eval_command (GtkWidget *widget, NomadAppWindow *window)
{
  GtkTextBuffer *buf;
  SCM key, proc, args, result, format;

  // text buffer
  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

  key = scm_call_0 (scm_c_public_ref ("nomad minibuffer", "current-command"));
  proc = scm_call_1 (scm_c_public_ref ("nomad eval", "command-ref"), key);
  args = scm_call_1 (scm_c_public_ref ("nomad eval", "command-args"), key);

  // If procedure does not take any arguments. Call the procedure and
  // set the minibuffer to either value returned from the procedure or
  // the exception
  if (scm_is_null (args))
    {
      result = scm_call_0 (proc);
      format = scm_call_3 (scm_c_public_ref ("guile", "format"), SCM_BOOL_F,
                           scm_from_utf8_string ("~a"), result);
      gtk_text_buffer_set_text (buf, scm_to_locale_string (format), -1);
      keyboard_quit (window);
      return;
    }
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

static void
nomad_app_window_overlay_init (NomadAppWindow *self)
{
  GtkWidget *scroll, *overlay_child;
  NomadAppWindowPrivate *priv = self->priv;

  priv->mini_popup = webkit_web_view_new ();
  priv->mini_frame = gtk_frame_new ("");

  // scroll window
  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_min_content_height (GTK_SCROLLED_WINDOW (scroll),
                                              250);
  gtk_scrolled_window_set_max_content_height (GTK_SCROLLED_WINDOW (scroll),
                                              250);

  // frame
  overlay_child = priv->mini_frame;

  gtk_container_add (GTK_CONTAINER (priv->mini_frame), priv->mini_popup);

  gtk_widget_set_size_request (overlay_child, 100, 150);

  gtk_widget_set_halign (overlay_child, GTK_ALIGN_FILL);
  gtk_widget_set_valign (overlay_child, GTK_ALIGN_END);
  gtk_widget_set_margin_bottom (overlay_child, 16);

  gtk_overlay_add_overlay (GTK_OVERLAY (priv->overlay), overlay_child);

  // Signals
  g_signal_connect (priv->mini_popup, "key-press-event",
                    G_CALLBACK (minipopup_key_press_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "show",
                    G_CALLBACK (minipopup_show_event_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "hide",
                    G_CALLBACK (minipopup_hide_event_cb), (gpointer)self);

  g_signal_connect (priv->mini_popup, "focus-out-event",
                    G_CALLBACK (minipopup_focus_out_event_cb), (gpointer)self);

  gtk_widget_hide (overlay_child);
}

static void
nomad_app_window_init (NomadAppWindow *self)
{
  NomadAppWindowPrivate *priv;
  NomadBuffer *buf;
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
  // Buffer
  buf = nomad_buffer_new ();
  webkit_web_view_load_uri (nomad_buffer_get_view (buf), c_home_page);
  nomad_app_window_add_buffer (self, buf);

  // Minbuf
  priv->text_buffer = GTK_WIDGET (text_buffer_new ());
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (priv->read_line),
                            GTK_TEXT_BUFFER (priv->text_buffer));

  // Signals

  // Main keypress
  g_signal_connect (self, "key-press-event", G_CALLBACK (window_key_press_cb),
                    (gpointer)self);

  g_signal_connect (priv->read_line, "focus-out-event",
                    G_CALLBACK (read_line_focus_out_event_cb), (gpointer)self);

  g_signal_connect (priv->read_line, "focus-in-event",
                    G_CALLBACK (read_line_focus_in_event_cb), (gpointer)self);

  g_signal_connect (priv->read_line, "key-release-event",
                    G_CALLBACK (read_line_key_release_event_cb),
                    (gpointer)self);

  g_signal_connect (priv->read_line, "key-press-event",
                    G_CALLBACK (read_line_key_press_event_cb), (gpointer)self);

  g_signal_connect (VTE_TERMINAL (priv->vte), "child-exited",
                    G_CALLBACK (fork_vte_child), NULL);

  gtk_widget_hide (self->priv->vte);

  // Cookies
  cookie_manager = webkit_web_context_get_cookie_manager (
      webkit_web_context_get_default ());

  webkit_cookie_manager_set_persistent_storage (
      cookie_manager, c_user_cookie_file,
      WEBKIT_COOKIE_PERSISTENT_STORAGE_SQLITE);

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

void
nomad_app_window_add_vte (NomadAppWindow *self)
{
  GdkRGBA b_rgba;
  GdkRGBA f_rgba;

  gdk_rgba_parse (&b_rgba, "white");
  gdk_rgba_parse (&f_rgba, "black");

  vte_terminal_set_color_background (VTE_TERMINAL (self->priv->vte), &b_rgba);
  vte_terminal_set_color_foreground (VTE_TERMINAL (self->priv->vte), &f_rgba);

  /* self->priv->vte = GTK_WIDGET (nomad_vte_new ()); */
  gtk_paned_add2 (GTK_PANED (self->priv->pane), self->priv->vte);
  fork_vte_child (VTE_TERMINAL (self->priv->vte), 0, NULL);
}

void
nomad_app_window_grab_vte (NomadAppWindow *self)
{
  gtk_widget_grab_focus (self->priv->vte);
}

void
nomad_app_window_show_vte (NomadAppWindow *self)
{
  gtk_widget_show (self->priv->vte);
}

void
nomad_app_window_hide_vte (NomadAppWindow *self)
{
  gtk_widget_hide (self->priv->vte);
}

GtkWidget *
nomad_app_window_get_minipopup (NomadAppWindow *self)
{
  return self->priv->mini_popup;
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

void
vte_show_cb (GtkWidget *vte, void *data)
{
  g_print ("SHOW\n");
}

void
nomad_app_window_start_repl (NomadAppWindow *self)
{
  fork_vte_child (VTE_TERMINAL (self->priv->vte), 0, NULL);
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
                                                NomadAppWindow, vte);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, read_line);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, notebook);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
                                                NomadAppWindow, overlay);
  gtk_widget_class_bind_template_child_private (
      GTK_WIDGET_CLASS (class), NomadAppWindow, mini_buffer_label);
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

SCM_DEFINE (scm_nomad_window_focus, "focus", 0, 0, 0, (),
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

void
nomad_window_register_functions (void *data)
{
#include "window.x"
  scm_c_export ("focus", NULL);
}
