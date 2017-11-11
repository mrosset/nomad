#ifndef __WEMACSAPPWIN_H
#define __WEMACSAPPWIN_H

#include <gtk/gtk.h>
#include "wemacsapp.h"


#define WEMACS_APP_WINDOW_TYPE (wemacs_app_window_get_type ())
G_DECLARE_FINAL_TYPE (WemacsAppWindow, wemacs_app_window, WEMACS, APP_WINDOW, GtkApplicationWindow)


WemacsAppWindow       *wemacs_app_window_new          (WemacsApp *app);

#endif /* __WEMACSAPPWIN_H */
