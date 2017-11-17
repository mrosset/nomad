/*
 * vte.c
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 *
 * wemacs is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * wemacs is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <vte/vte.h>

#include "app.h"
#include "vte.h"

G_DEFINE_TYPE (WemacsVte, wemacs_vte, VTE_TYPE_TERMINAL);
struct _WemacsVtePrivate
{
};

static void
fork_vte_child (VteTerminal * vte, gint status, gpointer data)
{
  gchar **envv;
  gchar *pwd;
  gchar *argv[] = { "emacs", "-nw", "-l", EMACS_INIT, NULL };

  pwd = g_get_current_dir ();
  envv = g_get_environ ();
  envv = g_environ_setenv (envv, "TERM", "xterm-256color", TRUE);

  vte_terminal_spawn_async (vte,
			    VTE_PTY_DEFAULT,
			    NULL,
			    argv,
			    envv,
			    G_SPAWN_DEFAULT | G_SPAWN_SEARCH_PATH_FROM_ENVP,
			    NULL, 0, NULL, -1, NULL, NULL, NULL);

  g_strfreev (envv);
  g_free (pwd);
}

static void
wemacs_vte_class_init (WemacsVteClass * class)
{
}

static void
wemacs_vte_init (WemacsVte * self)
{
  GdkRGBA b_rgba;
  GdkRGBA f_rgba;

  self->priv = wemacs_vte_get_instance_private (self);

  gdk_rgba_parse (&b_rgba, "white");
  gdk_rgba_parse (&f_rgba, "black");

  vte_terminal_set_color_background (VTE_TERMINAL (self->priv), &b_rgba);
  vte_terminal_set_color_foreground (VTE_TERMINAL (self->priv), &f_rgba);

  fork_vte_child (VTE_TERMINAL (self->priv), 0, NULL);
  g_signal_connect (self->priv, "child-exited", G_CALLBACK (fork_vte_child),
		    NULL);
}

WemacsVte *
wemacs_vte_new (void)
{
  return g_object_new (WEMACS_TYPE_VTE, NULL);
}
