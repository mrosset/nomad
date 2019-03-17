#include "app.h"

QVariant
invoke_method (QObject *object, const char *method)
{
  QVariant value;
  QMetaObject::invokeMethod (root, method, Qt::BlockingQueuedConnection,
                             Q_RETURN_ARG (QVariant, value));
  return value;
};
