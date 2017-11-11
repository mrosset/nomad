#include <gtk/gtk.h>

#include "wemacsapp.h"
#include "wemacsappwin.h"

struct _WemacsAppWindow
{
  GtkApplicationWindow parent;
};

G_DEFINE_TYPE(WemacsAppWindow, wemacs_app_window, GTK_TYPE_APPLICATION_WINDOW);

static void
wemacs_app_window_init (WemacsAppWindow *app)
{
}

static void
wemacs_app_window_class_init (WemacsAppWindowClass *class)
{
}

WemacsAppWindow *
wemacs_app_window_new (WemacsApp *app)
{
  return g_object_new (WEMACS_APP_WINDOW_TYPE, "application", app, NULL);
}
