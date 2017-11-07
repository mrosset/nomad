#include "emacs-module.h"
#include <string.h>
#include "emacs-module.h"

void emacs_message(emacs_env *env, const char *msg)
{
  emacs_value Qmessage = env->intern(env, "message");
  emacs_value Qstring = env->make_string(env, msg, strlen(msg));
  emacs_value pargs[] = { Qstring };
  env->funcall(env, Qmessage, 1, pargs);
}
