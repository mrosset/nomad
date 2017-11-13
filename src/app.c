#include <gtk/gtk.h>

#include "app.h"
#include "window.h"

struct _WemacsApp
{
  GtkApplication parent;
};

G_DEFINE_TYPE (WemacsApp, wemacs_app, GTK_TYPE_APPLICATION);

static void
wemacs_app_init (WemacsApp * app)
{
}

static void
wemacs_app_activate (GApplication * app)
{
  WemacsAppWindow *win;

  win = wemacs_app_window_new (WEMACS_APP (app));
  gtk_window_present (GTK_WINDOW (win));
}

static void
wemacs_app_class_init (WemacsAppClass * class)
{
  G_APPLICATION_CLASS (class)->activate = wemacs_app_activate;
}

WemacsApp *
wemacs_app_new (void)
{
  return g_object_new (WEMACS_APP_TYPE,
                       "application-id", "org.gnu.wemacsapp",
                       "flags", G_APPLICATION_HANDLES_OPEN, NULL);
}

WebKitWebView *
wemacs_app_get_webview (WemacsApp * app)
{
  GList *windows;
  WemacsAppWindow *win;
  WebKitWebView *webView;

  windows = gtk_application_get_windows (GTK_APPLICATION (app));

  if(!windows) {
          g_critical("could not find window");
          return NULL;
  }
  win = WEMACS_APP_WINDOW(windows->data);
  return wemacs_app_window_get_webview(win);
}
