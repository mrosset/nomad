/*
 * meta-plugin.c
 * Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Nomad
 *
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

#include "plugin.h"
#include "../config.h"
#include <clutter/clutter.h>

typedef struct _NomadShellPluginPrivate NomadShellPluginPrivate;

struct _NomadShellPluginPrivate
{
  /* SCM buffer; */
  const char *field;
};

struct _NomadShellPlugin
{
  MetaPlugin parent;
  NomadShellPluginPrivate *priv;
};

// clang-format off
G_DEFINE_TYPE_WITH_PRIVATE (NomadShellPlugin, nomad_shell_plugin, META_TYPE_PLUGIN);
// clang-format on

static void
nomad_shell_plugin_init (NomadShellPlugin *self)
{
  self->priv = nomad_shell_plugin_get_instance_private (self);
}

static void
nomad_shell_plugin_start (MetaPlugin *plugin)
{
  g_critical ("START");
}

static void
nomad_shell_plugin_map (MetaPlugin *plugin, MetaWindowActor *actor)
{
  ClutterActor *cactor = CLUTTER_ACTOR (meta_window_actor_get_texture (actor));
  g_critical ("MAP: %p", cactor);
}

static void
nomad_shell_plugin_minimize (MetaPlugin *plugin, MetaWindowActor *actor)
{
}

static void
nomad_shell_plugin_unminimize (MetaPlugin *plugin, MetaWindowActor *actor)
{
}

static void
nomad_shell_plugin_size_changed (MetaPlugin *plugin, MetaWindowActor *actor)
{
}

static void
nomad_shell_plugin_size_change (MetaPlugin *plugin, MetaWindowActor *actor,
                                MetaSizeChange which_change,
                                MetaRectangle *old_frame_rect,
                                MetaRectangle *old_buffer_rect)
{
}

static void
nomad_shell_plugin_destroy (MetaPlugin *plugin, MetaWindowActor *actor)
{
}

static void
nomad_shell_plugin_switch_workspace (MetaPlugin *plugin, gint from, gint to,
                                     MetaMotionDirection direction)
{
}

static void
nomad_shell_plugin_kill_window_effects (MetaPlugin *plugin,
                                        MetaWindowActor *actor)
{
}

static void
nomad_shell_plugin_kill_switch_workspace (MetaPlugin *plugin)
{
}

static void
nomad_shell_plugin_show_tile_preview (MetaPlugin *plugin, MetaWindow *window,
                                      MetaRectangle *tile_rect,
                                      int tile_monitor)
{
}

static void
nomad_shell_plugin_hide_tile_preview (MetaPlugin *plugin)
{
}

static void
nomad_shell_plugin_show_window_menu (MetaPlugin *plugin, MetaWindow *window,
                                     MetaWindowMenuType menu, int x, int y)
{
}

static void
nomad_shell_plugin_show_window_menu_for_rect (MetaPlugin *plugin,
                                              MetaWindow *window,
                                              MetaWindowMenuType menu,
                                              MetaRectangle *rect)
{
}

static gboolean
nomad_shell_plugin_xevent_filter (MetaPlugin *plugin, XEvent *xev)
{
  return FALSE;
}

static gboolean
nomad_shell_plugin_keybinding_filter (MetaPlugin *plugin,
                                      MetaKeyBinding *binding)
{
  return FALSE;
}

static void
nomad_shell_plugin_confirm_display_change (MetaPlugin *plugin)
{
}

static const MetaPluginInfo *
nomad_shell_plugin_plugin_info (MetaPlugin *plugin)
{
  static const MetaPluginInfo info
      = { .name = "Nomae Shell",
          .version = VERSION,
          .author = "Various",
          .license = "GPLv3+",
          .description = "Provides Nomad Shell core functionality" };

  return &info;
}

static MetaCloseDialog *
nomad_shell_plugin_create_close_dialog (MetaPlugin *plugin, MetaWindow *window)
{
  return NULL;
}

static MetaInhibitShortcutsDialog *
nomad_shell_plugin_create_inhibit_shortcuts_dialog (MetaPlugin *plugin,
                                                    MetaWindow *window)
{
  return NULL;
}

static void
nomad_shell_plugin_class_init (NomadShellPluginClass *klass)
{
  MetaPluginClass *plugin_class = META_PLUGIN_CLASS (klass);

  plugin_class->start = nomad_shell_plugin_start;
  plugin_class->map = nomad_shell_plugin_map;
  plugin_class->minimize = nomad_shell_plugin_minimize;
  plugin_class->unminimize = nomad_shell_plugin_unminimize;
  plugin_class->size_changed = nomad_shell_plugin_size_changed;
  plugin_class->size_change = nomad_shell_plugin_size_change;
  plugin_class->destroy = nomad_shell_plugin_destroy;

  plugin_class->switch_workspace = nomad_shell_plugin_switch_workspace;

  plugin_class->kill_window_effects = nomad_shell_plugin_kill_window_effects;
  plugin_class->kill_switch_workspace
      = nomad_shell_plugin_kill_switch_workspace;

  plugin_class->show_tile_preview = nomad_shell_plugin_show_tile_preview;
  plugin_class->hide_tile_preview = nomad_shell_plugin_hide_tile_preview;
  plugin_class->show_window_menu = nomad_shell_plugin_show_window_menu;
  plugin_class->show_window_menu_for_rect
      = nomad_shell_plugin_show_window_menu_for_rect;

  plugin_class->xevent_filter = nomad_shell_plugin_xevent_filter;
  plugin_class->keybinding_filter = nomad_shell_plugin_keybinding_filter;

  plugin_class->confirm_display_change
      = nomad_shell_plugin_confirm_display_change;

  plugin_class->plugin_info = nomad_shell_plugin_plugin_info;

  plugin_class->create_close_dialog = nomad_shell_plugin_create_close_dialog;
  plugin_class->create_inhibit_shortcuts_dialog
      = nomad_shell_plugin_create_inhibit_shortcuts_dialog;
}
