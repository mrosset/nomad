#include "app.h"

#include <QMetaMethod>
#include <QMetaObject>

void
print_methods (QObject *object)
{
  const QMetaObject *metaObj = object->metaObject ();
  for (int i = 0; i < metaObj->methodCount (); ++i)
    {
      QMetaMethod method = metaObj->method (i);
      qDebug () << method.methodSignature ();
    }
}

QVariant
invoke_method (QObject *object, const char *method)
{
  QVariant value;
  QMetaObject::invokeMethod (object, method, Qt::BlockingQueuedConnection,
                             Q_RETURN_ARG (QVariant, value));
  return value;
};
