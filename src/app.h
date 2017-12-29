#ifndef __NOMADAPP_H
#define __NOMADAPP_H

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define EMACS_INIT "/home/mrosset/src/nomad/init.el"

#define NOMAD_APP_TYPE (nomad_app_get_type ())
G_DECLARE_FINAL_TYPE (NomadApp, nomad_app, NOMAD, APP, GtkApplication)
NomadApp *nomad_app_new (void);
// NomadAppWindow  *nomad_app_get_window  (NomadApp *app);
WebKitWebView *nomad_app_get_webview (NomadApp *app);

#endif /* __NOMADAPP_H */
