/*
 * ext.c
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

 * This file is part of Nomad

 * Nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Nomad is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <webkit2/webkit-web-extension.h>
#include <libguile.h>
#include <sys/socket.h>

/*
static void
msg (void *data)
{
   SCM sock;
   SCM fam = scm_from_int (AF_UNIX);
   SCM addr = scm_from_locale_string ("/tmp/nomad");
   SCM message = scm_string_to_utf8 (scm_from_locale_string ("(version)"));
   scm_connect (sock, scm_from_int(AF_UNIX), scm_from_locale_string("/tmp/nomad"), SCM_EOL);
   scm_sendto (sock, message, fam, addr, 0);
}
*/

static void
web_page_loaded_callback (WebKitWebPage * web_page, gpointer user_data)
{
  g_print ("loaded: %s\n", webkit_web_page_get_uri (web_page));
  //scm_with_guile (msg, NULL);
}

static void
web_page_created_callback (WebKitWebExtension * extension,
         WebKitWebPage * web_page, char *user_data)
{
  g_print ("Page %d created\n", (int)webkit_web_page_get_id (web_page));
  g_signal_connect (web_page, "document-loaded",
        G_CALLBACK (web_page_loaded_callback), NULL);
}

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data (WebKitWebExtension *
            extension,
            GString * user_data)
{
  g_signal_connect (extension, "page-created",
        G_CALLBACK (web_page_created_callback), user_data);
}
