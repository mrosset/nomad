#include <libguile.h>
#include <gtk/gtk.h>
#include "app.h"

#define WEMACS_VERSION "0.1"

int plugin_is_GPL_compatible;


#define DEFAULT_URI "https://www.gnu.org/software/emacs"

static SCM
wemacs_start ()
{
  intmax_t status;
  GtkApplication *app;

  app = gtk_application_new ("org.gnu.wemacs", G_APPLICATION_FLAGS_NONE);

  g_signal_connect (app, "activate", G_CALLBACK (wemacs_activate), NULL);

  status = g_application_run (G_APPLICATION (app), 0, NULL);

  g_object_unref (app);
  return scm_from_intmax (status);
}

static SCM
wemacs_version ()
{
  return scm_from_locale_string (WEMACS_VERSION);
}

static void *
register_functions (void *data)
{
  scm_c_define_gsubr ("wemacs-version", 0, 0, 0, &wemacs_version);
  scm_c_define_gsubr ("wemacs-start", 0, 0, 0, &wemacs_start);
  return 0;
}

int
main (int argc, char **argv)
{
  scm_with_guile (&register_functions, NULL);
  scm_shell (argc, argv);
  return 0;
}
