#include "emacs-module.h"
#include "emacs.h"
#include "app.h"
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include <gtk/gtk.h>
#include <webkit2/webkit2.h>
#include <glib.h>

#define WEMACS_VERSION "0.1"

int plugin_is_GPL_compatible;

GtkApplication *app;

#define DEFAULT_URI "https://www.gnu.org/software/emacs"

static SCM
wemacs_start ()
{
  app = gtk_application_new ("org.gnu.wemacs", G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, "activate", G_CALLBACK (wemacs_activate), NULL);
  intmax_t status = g_application_run (G_APPLICATION (app), 0, NULL);
  g_object_unref (app);
  return scm_from_intmax (status);
}

static SCM
wemacs_version ()
{
  scm_from_locale_string (WEMACS_VERSION);

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

emacs_value
Fwemacs_stop (emacs_env * env, ptrdiff_t nargs,
        emacs_value args[], void *data)
{
  g_application_quit (G_APPLICATION (app));
}

static emacs_value
Fwemacs_start (emacs_env * env, ptrdiff_t nargs,
         emacs_value args[], void *data)
{
  /* GThread *thread; */

  /* thread = g_thread_new ("gtk", start_gtk, NULL); */
  /* g_thread_join(thread); */
  /* return env->make_integer (env, status); */
}

static emacs_value
Fwemacs_version (emacs_env * env, ptrdiff_t nargs,
     emacs_value args[], void *data)
{
  return env->make_string (env, WEMACS_VERSION, 3);
}

static emacs_value
Fwemacs_browse (emacs_env * env, ptrdiff_t nargs,
    emacs_value args[], void *data)
{
  GList *list = gtk_application_get_windows (app);

  if (g_list_length (list) == 1)
    {
      const char *msg = "only one";
      //return env->make_string (env, msg, strlen(msg));
    }
  return env->make_string (env, WEMACS_VERSION, 3);
}


static void
provide_feature (emacs_env * env)
{
  emacs_value Qfeat = env->intern (env, "wemacs");
  emacs_value Qprovide = env->intern (env, "provide");

  emacs_value pargs[] = { Qfeat };
  env->funcall (env, Qprovide, 1, pargs);
}

static void
fset_browse (emacs_env * env)
{
  emacs_value fn = env->make_function (env, 1, 1, Fwemacs_browse,
               "Internal browse URL", NULL);

  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, "wemacs-browse");

  emacs_value fargs[] = { Qsym, fn };
  env->funcall (env, Qfset, 2, fargs);
}

static void
fset_version (emacs_env * env)
{
  emacs_value fn = env->make_function (env, 0, 0, Fwemacs_version,
               "Returns wemacs version", NULL);

  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, "wemacs-version");

  emacs_value fargs[] = { Qsym, fn };
  env->funcall (env, Qfset, 2, fargs);
}

static void
fset_start (emacs_env * env)
{
  emacs_value fn = env->make_function (env, 0, 0, Fwemacs_start,
               "Start Wemacs", NULL);

  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, "wemacs-start");

  emacs_value fargs[] = { Qsym, fn };
  env->funcall (env, Qfset, 2, fargs);
}

extern int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  int status;

  provide_feature (env);
  fset_version (env);
  fset_start (env);
  fset_browse (env);
  return 0;
}
