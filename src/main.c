#include <gtk/gtk.h>

#include "wemacsapp.h"

int
main (int argc, char *argv[])
{
  return g_application_run (G_APPLICATION (wemacs_app_new ()), argc, argv);
}
