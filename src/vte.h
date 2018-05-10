/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-  */
/*
 * vte.h
 * Copyright (C) 2017 Mike Rosset <mike.rosset@gmail.com>
 *
 * nomad is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * nomad is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __NOMAD_VTE_H__
#define __NOMAD_VTE_H__

#include <glib-object.h>
#include <vte/vte.h>

#define EMACS_INIT "/home/mrosset/src/nomad/init.el"

G_BEGIN_DECLS
#define NOMAD_TYPE_VTE nomad_vte_get_type ()
#define NOMAD_VTE(obj)                                                        \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), NOMAD_TYPE_VTE, NomadVte))
#define NOMAD_VTE_CLASS(klass)                                                \
  (G_TYPE_CHECK_CLASS_CAST ((klass), NOMAD_TYPE_VTE, NomadVteClass))
#define NOMAD_IS_VTE(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), NOMAD_TYPE_VTE))
#define NOMAD_IS_VTE_CLASS(klass)                                             \
  (G_TYPE_CHECK_CLASS_TYPE ((klass), NOMAD_TYPE_VTE))
#define NOMAD_VTE_GET_CLASS(obj)                                              \
  (G_TYPE_INSTANCE_GET_CLASS ((obj), NOMAD_TYPE_VTE, NomadVteClass))
typedef struct _NomadVte NomadVte;
typedef struct _NomadVteClass NomadVteClass;
typedef struct _NomadVtePrivate NomadVtePrivate;

struct _NomadVte
{
  VteTerminal parent;

  NomadVtePrivate *priv;
};

struct _NomadVteClass
{
  VteTerminal parent_instance;
  VteTerminalClass parent_class;
};

GType nomad_vte_get_type (void) G_GNUC_CONST;

NomadVte *nomad_vte_new (void);

G_END_DECLS
#endif /* __NOMAD_VTE_H__ */
