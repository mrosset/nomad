#ifndef __NOMADAPP_H
#define __NOMADAPP_H

#include "buffer.h"
#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#define EMACS_INIT "/home/mrosset/src/nomad/init.el"
#define MAX_BUFFERS 2

#define NOMAD_APP_TYPE (nomad_app_get_type ())
G_DECLARE_FINAL_TYPE (NomadApp, nomad_app, NOMAD, APP, GtkApplication)
NomadApp *nomad_app_new (void);
WebKitWebView *nomad_app_get_webview (NomadApp *app);
NomadBuffer *nomad_app_get_first_buffer (NomadApp *self);
#endif /* __NOMADAPP_H */
