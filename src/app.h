#ifndef __WEMACSAPP_H
#define __WEMACSAPP_H

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define EMACS_INIT  "/home/mrosset/src/wemacs/init.el"

#define WEMACS_APP_TYPE (wemacs_app_get_type ())
G_DECLARE_FINAL_TYPE (WemacsApp, wemacs_app, WEMACS, APP, GtkApplication)

WemacsApp        *wemacs_app_new         (void);
//WemacsAppWindow  *wemacs_app_get_window  (WemacsApp *app);
WebKitWebView    *wemacs_app_get_webview (WemacsApp *app);

#endif /* __WEMACSAPP_H */
