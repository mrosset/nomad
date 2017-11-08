#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <webkit2/webkit2.h>

#define DEFAULT_URI    "https://www.gnu.org/software/emacs"


GtkWidget *minbuf;
WebKitWebView *webView;

void
close_minibuffer ()
{
  gtk_entry_set_text (GTK_ENTRY (minbuf), "");
  gtk_widget_hide (minbuf);
}

gboolean
on_key_press (GtkWidget * widget, GdkEventKey * event)
{
  GdkModifierType modifiers;

  modifiers = gtk_accelerator_get_default_mod_mask ();

  if (event->keyval == GDK_KEY_Return && gtk_widget_has_focus (minbuf))
    {
      GString *input = g_string_new (gtk_entry_get_text (GTK_ENTRY (minbuf)));

      close_minibuffer();

      if (input->len != 0) {
        webkit_web_view_load_uri (webView, input->str);
      }


      g_string_free(input,TRUE);
      return TRUE;
    }

  if (event->keyval == GDK_KEY_x
      && (event->state & modifiers) == GDK_MOD1_MASK)
    {
      gtk_widget_show (minbuf);
      gtk_widget_grab_focus (minbuf);
      return TRUE;
    }

  return FALSE;
}

void
wemacs_activate (GtkApplication * app, gpointer user_data)
{
  GtkWidget *window;
  GtkWidget *box;


  window = gtk_application_window_new (app);
  gtk_window_set_title (GTK_WINDOW (window), "Wemacs");

  webView = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  webkit_web_view_load_uri (webView, DEFAULT_URI);

  box = gtk_box_new (TRUE, 1);

  gtk_container_add (GTK_CONTAINER (window), box);
  gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (webView), TRUE, TRUE, 0);

  minbuf = gtk_entry_new ();
  gtk_box_pack_start (GTK_BOX (box), minbuf, FALSE, TRUE, 0);

  g_signal_connect (G_OBJECT (window), "key-press-event",
        G_CALLBACK (on_key_press), NULL);

  gtk_widget_show_all (window);
  gtk_widget_hide (minbuf);
}
