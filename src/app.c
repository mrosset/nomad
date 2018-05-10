#include <gtk/gtk.h>
#include <libguile.h>

#include "app.h"
#include "window.h"

struct _NomadApp
{
  GtkApplication parent;
};

typedef struct _NomadAppPrivate NomadAppPrivate;

struct _NomadAppPrivate
{
  GList *buffers;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadApp, nomad_app, GTK_TYPE_APPLICATION);

static void
nomad_app_init (NomadApp *app)
{
}

static void
nomad_app_activate (GApplication *app)
{

  NomadAppWindow *win;
  NomadAppPrivate *priv = nomad_app_get_instance_private (NOMAD_APP (app));
  /* char *c_home_page; */
  /* SCM home_page; */

  /* scm_dynwind_begin (0); */

  /* home_page = scm_c_public_ref ("nomad browser", "default-home-page"); */
  /* c_home_page = scm_to_locale_string (home_page); */

  /* priv->buffers = NULL; */
  win = nomad_app_window_new (NOMAD_APP (app));
  /* for (int i = 0; i <= MAX_BUFFERS; i++) */
  /*   { */
  /*     WebKitWebView *view = WEBKIT_WEB_VIEW (webkit_web_view_new ()); */
  /*     priv->buffers = g_list_append (priv->buffers, view); */
  /*   } */

  /* // Home Page */
  /* webkit_web_view_load_uri (g_list_first (priv->buffers)->data,
   * c_home_page); */
  /* nomad_app_window_replace_webview (win, g_list_first
   * (priv->buffers)->data); */
  /* scm_dynwind_free (c_home_page); */
  /* scm_dynwind_end (); */
  /* nomad_app_print_buffers (NOMAD_APP (app)); */
  gtk_window_present (GTK_WINDOW (win));
}

static void
nomad_app_class_init (NomadAppClass *class)
{
  G_APPLICATION_CLASS (class)->activate = nomad_app_activate;
}

NomadApp *
nomad_app_new (void)
{
  return g_object_new (NOMAD_APP_TYPE, "application-id", "org.gnu.nomadapp",
                       "flags", G_APPLICATION_HANDLES_OPEN, NULL);
}

NomadAppWindow *
nomad_app_get_window (NomadApp *app)
{

  GList *windows;

  windows = gtk_application_get_windows (GTK_APPLICATION (app));

  if (!windows)
    {
      g_critical ("could not find window");
      return NULL;
    }
  return NOMAD_APP_WINDOW (windows->data);
}

void
nomad_app_switch_to_buffer (NomadApp *app, const char *uri)
{

  /* NomadAppWindow *win; */
  /* GtkWidget *box; */
  /* WebKitWebView *view; */
  /* NomadAppPrivate *priv = nomad_app_get_instance_private (app); */

  /* win = nomad_app_get_window (app); */
  /* box = nomad_app_window_get_box (win); */
  /* view = g_list_first (priv->buffers)->data; */
  /* webkit_web_view_load_uri (view, uri); */
  /* gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (view), TRUE, TRUE, 0); */
  /* nomad_app_window_set_webview (win, view); */
  /* gtk_widget_show_all (box); */
}

void
nomad_app_print_buffers (NomadApp *app)
{

  GList *l;
  NomadAppPrivate *priv;

  priv = nomad_app_get_instance_private (app);

  for (l = priv->buffers; l != NULL; l = l->next)
    {
      g_print ("point: %p title: %s url: %s\n", l->data,
               webkit_web_view_get_title (l->data),
               webkit_web_view_get_uri (l->data));
    }
}

WebKitWebView *
nomad_app_get_current_buffer (NomadApp *app)
{

  NomadAppPrivate *priv;

  priv = nomad_app_get_instance_private (app);
  return priv->buffers->data;
}

void
nomad_app_next_buffer (NomadApp *app)
{

  NomadAppWindow *win;
  WebKitWebView *view;
  NomadAppPrivate *priv = nomad_app_get_instance_private (app);

  win = nomad_app_get_window (app);
  if (!priv->buffers->next)
    {
      priv->buffers = g_list_first (priv->buffers);
    }
  else
    {
      priv->buffers = priv->buffers->next;
    }
  view = priv->buffers->data;

  nomad_app_window_replace_webview (win, view);
}

WebKitWebView *
nomad_app_get_webview (NomadApp *app)
{
  GList *windows;
  NomadAppWindow *win;

  windows = gtk_application_get_windows (GTK_APPLICATION (app));

  if (!windows)
    {
      g_critical ("could not find window");
      return NULL;
    }
  win = NOMAD_APP_WINDOW (windows->data);
  return nomad_app_window_get_webview (win);
}
