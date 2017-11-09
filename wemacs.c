#include <libguile.h>
#include <gtk/gtk.h>
#include "app.h"

#define WEMACS_VERSION "0.1"

#define DEFAULT_URI "https://www.gnu.org/software/emacs"

GtkApplication *app;

SCM search (SCM q)
{
  GString *url = g_string_new("");
  gchar *cq = scm_to_locale_string(q);
  g_string_printf(url, "https://google.com/search?q=%s", cq);
  webkit_web_view_load_uri (webView, url->str);
  gtk_widget_hide (minbuf);
  return scm_from_locale_string(url->str);
}

SCM
web_view_load_uri(SCM url)
{
  char *curl = scm_to_locale_string (url);
  webkit_web_view_load_uri (webView, curl);
  return url;
}

SCM web_view_reload()
{
  webkit_web_view_reload (webView);
  return SCM_BOOL_T;
}

SCM
web_view_go_forward () {
  webkit_web_view_go_forward (webView);
  return SCM_BOOL_T;
}

SCM
web_view_go_back () {
  webkit_web_view_go_back (webView);
  return SCM_BOOL_T;
}

SCM close_emacs () {
  gtk_widget_hide (minbuf);
  return SCM_BOOL_T;
}

SCM
wemacs_quit ()
{
  g_application_quit (G_APPLICATION(app));
  return  SCM_BOOL_T;
}

SCM
wemacs_start ()
{
  intmax_t status;
  app = gtk_application_new ("org.gnu.wemacs", G_APPLICATION_FLAGS_NONE);

  g_signal_connect (app, "activate", G_CALLBACK (wemacs_activate), NULL);

  status = g_application_run (G_APPLICATION (app), 0, NULL);

  g_object_unref (app);
  return scm_from_intmax (status);
}

SCM
wemacs_version ()
{
  return scm_from_locale_string (WEMACS_VERSION);
}

static void *
register_functions (void *data)
{
  scm_c_define_gsubr ("wemacs-version", 0, 0, 0, &wemacs_version);
  scm_c_define_gsubr ("wemacs-start", 0, 0, 0, &wemacs_start);
  scm_c_define_gsubr ("wemacs-quit", 0, 0, 0, &wemacs_quit);
  scm_c_define_gsubr ("close-emacs", 0, 0, 0, &close_emacs);
  scm_c_define_gsubr ("web-view-load-uri", 1, 0, 0, &web_view_load_uri);
  scm_c_define_gsubr ("web-view-go-back", 0, 0, 0, &web_view_go_back);
  scm_c_define_gsubr ("web-view-go-forward", 0, 0, 0, &web_view_go_forward);
  scm_c_define_gsubr ("web-view-reload", 0, 0, 0, &web_view_reload);
}

int
main (int argc, char **argv)
{
  scm_with_guile (&register_functions, NULL);
  scm_shell (argc, argv);
  return 0;
}
