#include <gtk/gtk.h>

#include "wemacsapp.h"
#include "wemacsappwin.h"

struct _WemacsAppWindow
{
  GtkApplicationWindow parent;
};

G_DEFINE_TYPE(WemacsAppWindow, wemacs_app_window, GTK_TYPE_APPLICATION_WINDOW);

static void
wemacs_app_window_init (WemacsAppWindow *win)
{
  gtk_widget_init_template (GTK_WIDGET (win));
}

static void
wemacs_app_window_class_init (WemacsAppWindowClass *class)
{
gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
                                               "/org/gnu/wemacseapp/window.ui");
}

WemacsAppWindow *
wemacs_app_window_new (WemacsApp *app)
{
  return g_object_new (WEMACS_APP_WINDOW_TYPE, "application", app, NULL);
}
