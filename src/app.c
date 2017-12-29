#include <gtk/gtk.h>

#include "app.h"
#include "window.h"

struct _NomadApp
{
  GtkApplication parent;
};

G_DEFINE_TYPE (NomadApp, nomad_app, GTK_TYPE_APPLICATION);

static void
nomad_app_init (NomadApp *app)
{
}

static void
nomad_app_activate (GApplication *app)
{
  NomadAppWindow *win;

  win = nomad_app_window_new (NOMAD_APP (app));
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
