/* nomad-buffer.c */

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

static void
nomad_buffer_init (NomadBuffer *self)
{
  NomadBufferPrivate *priv;

  gtk_widget_init_template (GTK_WIDGET (self));

  self->priv = nomad_buffer_get_instance_private(self);

  priv = self->priv;
  if(!priv->status)
    {
      g_critical("status NULL");
    }
  priv->view = WEBKIT_WEB_VIEW(webkit_web_view_new());
  webkit_web_view_load_uri(priv->view, "http://gnu.org");
  gtk_box_pack_start(self, GTK_WIDGET(priv->view), TRUE, TRUE, 0);
  gtk_box_reorder_child(self, GTK_WIDGET(priv->view), 0);

  g_signal_connect (priv->view, "load-changed", G_CALLBACK (web_view_load_changed),
                    priv->status);

}

static void
nomad_buffer_class_init (NomadBufferClass *klass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (klass),
                                               "/org/gnu/nomadapp/buffer.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (klass),
                                                NomadBuffer, status);
}

NomadBuffer *
nomad_buffer_new (void)
{
  return g_object_new (NOMAD_TYPE_BUFFER, NULL);
}
