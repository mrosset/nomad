/*
 * buffer.h
 * Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>
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

#ifndef __NOMAD_BUFFER_H__
#define __NOMAD_BUFFER_H__

#include <gtk/gtk.h>
#include <libguile.h>
#include <webkit2/webkit2.h>

G_BEGIN_DECLS

#define NOMAD_TYPE_BUFFER nomad_buffer_get_type ()
G_DECLARE_FINAL_TYPE (NomadBuffer, nomad_buffer, NOMAD, BUFFER, GtkBox)

typedef struct _NomadBuffer NomadBuffer;
typedef struct _NomadBufferPrivate NomadBufferPrivate;

struct _NomadBuffer
{
  GtkBox parent;

  NomadBufferPrivate *priv;
};

struct _NomadBufferClass
{
  GtkBoxClass parent_class;
};

struct buffer
{
  WebKitWebView *view;
};

SCM buffer_type;

GType nomad_buffer_get_type (void) G_GNUC_CONST;

NomadBuffer *nomad_buffer_new (void);
WebKitWebView *nomad_buffer_get_view (NomadBuffer *buf);
GtkLabel *nomad_buffer_get_status (NomadBuffer *buf);
GtkWidget *nomad_buffer_get_title (NomadBuffer *buf);
G_END_DECLS

#endif /* __NOMAD_BUFFER_H__ */
