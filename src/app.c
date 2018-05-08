#include <libguile.h>
#include <gtk/gtk.h>

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
  char *c_home_page;
  SCM home_page;

  scm_dynwind_begin (0);

  home_page = scm_c_public_ref ("nomad browser", "default-home-page");
  c_home_page = scm_to_locale_string (home_page);

  priv->buffers = NULL;
  win = nomad_app_window_new (NOMAD_APP (app));
  for (int i = 0; i <= MAX_BUFFERS; i++)
    {
      WebKitWebView *view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
      priv->buffers = g_list_append (priv->buffers,  view);
    }

  // Home Page
  nomad_app_switch_to_buffer(NOMAD_APP(app), c_home_page);
  scm_dynwind_free (c_home_page);
  scm_dynwind_end();
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
nomad_app_get_window(NomadApp *app) {

  GList *windows;

  windows = gtk_application_get_windows (GTK_APPLICATION (app));

  if (!windows)
    {
      g_critical ("could not find window");
      return NULL;
    }
  return NOMAD_APP_WINDOW (windows->data);
}

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

void
nomad_app_switch_to_buffer(NomadApp *app, const char *uri) {

  NomadAppWindow *win;
  GtkWidget *box;
  WebKitWebView *view;
  NomadAppPrivate *priv = nomad_app_get_instance_private(app);

  win = nomad_app_get_window(app);
  box = nomad_app_window_get_box(win);
  view = g_list_first(priv->buffers)->data;
  webkit_web_view_load_uri(view, uri);
  gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (view), TRUE,
                      TRUE, 0);
  g_signal_connect (view, "load-changed",
                    G_CALLBACK (web_view_load_changed), nomad_app_window_get_status(win));

  nomad_app_window_set_webview(win, view);
  gtk_widget_show_all(box);

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
