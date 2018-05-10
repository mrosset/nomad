/* nomad-buffer.h */

#ifndef __NOMAD_BUFFER_H__
#define __NOMAD_BUFFER_H__

#include <gtk/gtk.h>
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

GType nomad_buffer_get_type (void) G_GNUC_CONST;

NomadBuffer *nomad_buffer_new (void);
WebKitWebView *nomad_buffer_get_view (NomadBuffer *buf);
G_END_DECLS

#endif /* __NOMAD_BUFFER_H__ */
