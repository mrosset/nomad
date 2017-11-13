#ifndef __WEMACSAPP_H
#define __WEMACSAPP_H

#include <gtk/gtk.h>


#define EMACS_INIT  "/home/mrosset/src/wemacs/init.el"

#define WEMACS_APP_TYPE (wemacs_app_get_type ())
G_DECLARE_FINAL_TYPE (WemacsApp, wemacs_app, WEMACS, APP, GtkApplication)

WemacsApp     *wemacs_app_new         (void);


#endif /* __WEMACSAPP_H */
