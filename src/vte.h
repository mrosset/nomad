/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-  */
/*
 * vte.h
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
#ifndef __WEMACS_VTE_H__
#define __WEMACS_VTE_H__

#include <glib-object.h>
#include <vte/vte.h>

#define EMACS_INIT "/home/mrosset/src/wemacs/init.el"

G_BEGIN_DECLS
#define WEMACS_TYPE_VTE wemacs_vte_get_type ()
#define WEMACS_VTE(obj)                                                       \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), WEMACS_TYPE_VTE, WemacsVte))
#define WEMACS_VTE_CLASS(klass)                                               \
  (G_TYPE_CHECK_CLASS_CAST ((klass), WEMACS_TYPE_VTE, WemacsVteClass))
#define WEMACS_IS_VTE(obj)                                                    \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), WEMACS_TYPE_VTE))
#define WEMACS_IS_VTE_CLASS(klass)                                            \
  (G_TYPE_CHECK_CLASS_TYPE ((klass), WEMACS_TYPE_VTE))
#define WEMACS_VTE_GET_CLASS(obj)                                             \
  (G_TYPE_INSTANCE_GET_CLASS ((obj), WEMACS_TYPE_VTE, WemacsVteClass))
typedef struct _WemacsVte WemacsVte;
typedef struct _WemacsVteClass WemacsVteClass;
typedef struct _WemacsVtePrivate WemacsVtePrivate;

struct _WemacsVte
{
  VteTerminal parent;

  WemacsVtePrivate *priv;
};

struct _WemacsVteClass
{
  VteTerminal parent_instance;
  VteTerminalClass parent_class;
};

GType wemacs_vte_get_type (void) G_GNUC_CONST;

WemacsVte *wemacs_vte_new (void);

G_END_DECLS
#endif /* __WEMACS_VTE_H__ */
