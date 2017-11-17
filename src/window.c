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
#include <webkit2/webkit2.h>

#include "app.h"
#include "window.h"
#include "vte.h"

struct _WemacsAppWindow
{
  GtkApplicationWindow parent;
};

typedef struct _WemacsAppWindowPrivate WemacsAppWindowPrivate;

struct _WemacsAppWindowPrivate
{
  GtkBox *box;
  GtkWidget *statusbar;
  GtkWidget *vte;
  WebKitWebView *webView;
  GList *links;
};

G_DEFINE_TYPE_WITH_PRIVATE (WemacsAppWindow, wemacs_app_window,
			    GTK_TYPE_APPLICATION_WINDOW);

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

  WemacsAppWindowPrivate *win =
    wemacs_app_window_get_instance_private (WEMACS_APP_WINDOW (widget));
  GtkWidget *vte = win->vte;

  gtk_widget_grab_focus (vte);
  modifiers = gtk_accelerator_get_default_mod_mask ();

  if (event->keyval == GDK_KEY_m &&
      (event->state & modifiers) == GDK_MOD1_MASK)
    {

      if (!gtk_widget_is_visible (vte))
	{
	  gtk_widget_show (vte);
	  gtk_widget_grab_focus (vte);
	  return TRUE;
	}

      if (gtk_widget_is_visible (vte) && gtk_widget_is_focus (vte))
	{
	  gtk_widget_hide (vte);
	  return TRUE;
	}


      if (gtk_widget_is_visible (vte) && !gtk_widget_is_focus (vte))
	{
	  gtk_widget_grab_focus (vte);
	  return TRUE;
	}

      return FALSE;
    }

  if (event->keyval == GDK_KEY_x &&
      (event->state & modifiers) == GDK_MOD1_MASK)
    {

      if (!gtk_widget_is_visible (vte))
	{
	  gtk_widget_show (vte);
	  gtk_widget_grab_focus (vte);
	  return TRUE;
	}

      return FALSE;
    }

  return FALSE;
}

static void
initialize_web_extensions (WebKitWebContext * context, gpointer user_data)
{
  /* Web Extensions get a different ID for each Web Process */
  static guint32 unique_id = 1;

  webkit_web_context_set_web_extensions_directory (context,
						   WEMACS_WEB_EXTENSIONS_DIR);
  webkit_web_context_set_web_extensions_initialization_user_data (context,
								  g_variant_new_uint32
								  (unique_id++));
}

static void
wemacs_app_window_init (WemacsAppWindow * win)
{

  WemacsAppWindowPrivate *priv;
  GString *test;

  gtk_widget_init_template (GTK_WIDGET (win));

  priv = wemacs_app_window_get_instance_private (win);
  priv->vte = GTK_WIDGET (wemacs_vte_new ());
  priv->links = NULL;
  priv->links = g_list_append (priv->links, g_string_new ("test"));
  test = (GString *) g_list_first (priv->links)->data;
  g_print ("window: %s\n", test->str);
  g_signal_connect (webkit_web_context_get_default (),
		    "initialize-web-extensions",
		    G_CALLBACK (initialize_web_extensions),
		    g_string_new ("test"));

  priv->webView = WEBKIT_WEB_VIEW (webkit_web_view_new ());

  // Signals
  g_signal_connect (priv->webView, "load-changed",
		    G_CALLBACK (web_view_load_changed), priv->statusbar);

  g_signal_connect (win, "key-press-event", G_CALLBACK (on_key_press), NULL);

  gtk_box_pack_start (priv->box, GTK_WIDGET (priv->webView), TRUE, TRUE, 0);
  gtk_box_pack_start (priv->box, GTK_WIDGET (priv->vte), FALSE, TRUE, 0);

  // Reorder
  gtk_box_reorder_child (priv->box, GTK_WIDGET (priv->webView), 0);
  gtk_box_reorder_child (priv->box, GTK_WIDGET (priv->vte), 1);

  gtk_widget_show_all (GTK_WIDGET (priv->box));
  gtk_widget_hide (GTK_WIDGET (priv->vte));
  webkit_web_view_load_uri (priv->webView, DEFAULT_URI);
}

static void
wemacs_app_window_class_init (WemacsAppWindowClass * class)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
					       "/org/gnu/wemacseapp/window.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
						WemacsAppWindow, box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (class),
						WemacsAppWindow, statusbar);
  g_type_class_add_private (class, sizeof (WemacsAppWindowPrivate));
}

WemacsAppWindow *
wemacs_app_window_new (WemacsApp * app)
{
  return g_object_new (WEMACS_APP_WINDOW_TYPE, "application", app, NULL);
}


WebKitWebView *
wemacs_app_window_get_webview (WemacsAppWindow * win)
{
  WemacsAppWindowPrivate *priv;

  priv = wemacs_app_window_get_instance_private (win);

  return priv->webView;
}
