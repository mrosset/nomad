#include "emacs-module.h"
#include <stdio.h>

#define WEMACS_VERSION "0.1"

int plugin_is_GPL_compatible;

static emacs_value Fwemacs_version(emacs_env * env, ptrdiff_t nargs,
           emacs_value args[], void *data)
{
  return env->make_string(env, WEMACS_VERSION, 3);
}

static void provide_feature(emacs_env * env)
{
  emacs_value Qfeat = env->intern(env, "wemacs");
  emacs_value Qprovide = env->intern(env, "provide");

  emacs_value pargs[] = { Qfeat };
  env->funcall(env, Qprovide, 1, pargs);
}

static void fset_functions(emacs_env * env)
{
  emacs_value fn = env->make_function(env, 0, 0, Fwemacs_version,
              "Returns wemacs version", NULL);

  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, "wemacs-version");

  emacs_value fargs[] = { Qsym, fn };
  env->funcall(env, Qfset, 2, fargs);
}

extern int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  provide_feature(env);
  fset_functions(env);
  return 0;
}
