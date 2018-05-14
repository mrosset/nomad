/*
 * buffer.c
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

#include "buffer.h"
#include <webkit2/webkit2.h>

struct _NomadBufferPrivate
{
  WebKitWebView *view;
  GtkWidget *status;
};

G_DEFINE_TYPE_WITH_PRIVATE (NomadBuffer, nomad_buffer, GTK_TYPE_BOX)

static void
web_view_load_changed (WebKitWebView *web_view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
  GtkLabel *label;
  const gchar *uri;

  uri = webkit_web_view_get_uri (web_view);
  label = GTK_LABEL (user_data);
  gtk_label_set_text (label, uri);
}

void
nomad_buffer_set_view (NomadBuffer *self, WebKitWebView *view)
{
}

static void
nomad_buffer_init (NomadBuffer *self)
{
  NomadBufferPrivate *priv;

  gtk_widget_init_template (GTK_WIDGET (self));

  self->priv = nomad_buffer_get_instance_private (self);

  priv = self->priv;
  priv->view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  gtk_box_pack_start (GTK_BOX (self), GTK_WIDGET (priv->view), TRUE, TRUE, 0);
  gtk_box_reorder_child (GTK_BOX (self), GTK_WIDGET (priv->view), 0);

  g_signal_connect (priv->view, "load-changed",
                    G_CALLBACK (web_view_load_changed), priv->status);
}

static void
nomad_buffer_class_init (NomadBufferClass *klass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
                                               "/org/gnu/nomad/buffer.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, status);
}

WebKitWebView *
nomad_buffer_get_view (NomadBuffer *buf)
{
  return buf->priv->view;
}

GtkLabel *
nomad_buffer_get_status (NomadBuffer *buf)
{
  return GTK_LABEL (buf->priv->status);
}

NomadBuffer *
nomad_buffer_new (void)
{
  return g_object_new (NOMAD_TYPE_BUFFER, NULL);
}
