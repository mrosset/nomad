/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*-  */
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkx.h>
#include <libguile.h>
#include <vte/vte.h>
#include <webkit2/webkit2.h>

#define DEFAULT_URI "https://www.gnu.org/software/emacs"
#define EMACS_INIT  "/home/mrosset/src/wemacs/init.el"

GtkWidget *minbuf;
WebKitWebView *webView;

void
my_getsize (GtkWidget * widget, GtkAllocation * allocation, void *data)
{
  printf ("width = %d, height = %d\n", allocation->width, allocation->height);
}

static void
web_view_load_changed (WebKitWebView * web_view,
           WebKitLoadEvent load_event, gpointer user_data)
{
  GtkLabel *label = GTK_LABEL (user_data);
  const gchar *uri = webkit_web_view_get_uri (web_view);
  gtk_label_set_text (label, uri);

  switch (load_event)
    {
    case WEBKIT_LOAD_STARTED:
      /* New load, we have now a provisional URI */
      /* Here we could start a spinner or update the
       * location bar with the provisional URI */
      break;
    case WEBKIT_LOAD_REDIRECTED:
      break;
    case WEBKIT_LOAD_COMMITTED:
      /* The load is being performed. Current URI is
       * the final one and it won't change unless a new
       * load is requested or a navigation within the
       * same page is performed */
      break;
    case WEBKIT_LOAD_FINISHED:
      /* Load finished, we can now stop the spinner */
      break;
    }
}

gboolean
on_key_press (GtkWidget * widget, GdkEventKey * event)
{
  GdkModifierType modifiers;

  modifiers = gtk_accelerator_get_default_mod_mask ();

  if (event->keyval == GDK_KEY_m &&
      (event->state & modifiers) == GDK_MOD1_MASK)
    {

      if (!gtk_widget_is_visible (minbuf))
  {
    gtk_widget_show (minbuf);
    gtk_widget_grab_focus (minbuf);
    return TRUE;
  }

      if (gtk_widget_is_visible (minbuf) && gtk_widget_is_focus (minbuf))
  {
    gtk_widget_hide (minbuf);
    return TRUE;
  }


      if (gtk_widget_is_visible (minbuf) && !gtk_widget_is_focus (minbuf))
  {
    gtk_widget_grab_focus (minbuf);
    return TRUE;
  }

      return FALSE;
    }

  if (event->keyval == GDK_KEY_x &&
      (event->state & modifiers) == GDK_MOD1_MASK)
    {

      if (!gtk_widget_is_visible (minbuf))
  {
    gtk_widget_show (minbuf);
    gtk_widget_grab_focus (minbuf);
    return TRUE;
  }

      return FALSE;
    }

  return FALSE;
}

void
child_setup (void *data)
{
}

void
fork_vte_child (VteTerminal * vte, gpointer data)
{
  const char *home;
  char **envv;

  g_autoptr (GError) error = NULL;

  gchar *pwd = g_get_current_dir ();

  char *argv[] = { "emacs", "-nw", "-l", EMACS_INIT, NULL };
  /* char *argv[] = { "bash", "--login", NULL }; */

  envv = g_get_environ ();

  /* vte_terminal_fork_command_full (VTE_TERMINAL (vte), VTE_PTY_DEFAULT, home, */
  /*         argv, envv, */
  /*         G_SPAWN_DEFAULT | G_SPAWN_SEARCH_PATH, */
  /*         child_setup, 0, NULL, &error); */

  vte_terminal_spawn_async (vte,
          VTE_PTY_DEFAULT,
          NULL,
          argv,
          envv,
          G_SPAWN_DEFAULT | G_SPAWN_SEARCH_PATH_FROM_ENVP,
          NULL, 0, NULL, -1, NULL, NULL, NULL);

  g_free (pwd);
}

GtkWidget *
make_vte ()
{
  GtkWidget *vte = vte_terminal_new ();

  GdkRGBA b_rgba;
  GdkRGBA f_rgba;

  gdk_rgba_parse (&b_rgba, "white");
  gdk_rgba_parse (&f_rgba, "black");

  vte_terminal_set_color_background (VTE_TERMINAL (vte), &b_rgba);
  vte_terminal_set_color_foreground (VTE_TERMINAL (vte), &f_rgba);

  fork_vte_child (VTE_TERMINAL (vte), NULL);
  g_signal_connect (vte, "child-exited", G_CALLBACK (fork_vte_child), NULL);

  return vte;
}

void
wemacs_activate (GtkApplication * app, gpointer user_data)
{
  GtkWidget *window;
  GtkWidget *box;
  GtkWidget *modeline;

  // webView
  webView = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  webkit_web_view_load_uri (webView, DEFAULT_URI);

  // Window
  window = gtk_application_window_new (app);
  gtk_window_set_title (GTK_WINDOW (window), "Wemacs");

  // Box
  box = gtk_box_new (TRUE, 1);
  gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
  // Modeline
  modeline = gtk_label_new ("");

  // Vte
  minbuf = make_vte ();

  // Signals


  g_signal_connect (webView, "load-changed",
        G_CALLBACK (web_view_load_changed), modeline);

  g_signal_connect (G_OBJECT (window), "key-press-event",
        G_CALLBACK (on_key_press), NULL);

  // Pack box
  gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (webView), TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (box), minbuf, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (box), modeline, FALSE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (window), box);
  gtk_widget_show_all (window);
  gtk_widget_hide (minbuf);
}
