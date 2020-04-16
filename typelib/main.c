#include "wayland.h"
#include <glib-object.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
  NomadWaylandClient *client = g_object_new (NOMAD_TYPE_WAYLAND_CLIENT, NULL);

  /* setenv ("WAYLAND_DISPLAY", "wayland-0", 1); */
  /* setenv ("WAYLAND_BACKEND", "wayland", 1); */

  nomad_wayland_server_start ();
  nomad_wayland_client_connect (client);
  nomad_wayland_client_disconnect (client);
  return EXIT_SUCCESS;
}
