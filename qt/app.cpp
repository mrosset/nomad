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
      int id = method.returnType ();
      const char *type = QMetaType::typeName (id);
      qDebug () << type << method.methodSignature ();
    }
}

SCM
qstring_to_scm (QString text)
{
  return scm_from_utf8_string (text.toUtf8 ().constData ());
}

QString
scm_to_qstring (SCM text)
{
  return QString (scm_to_utf8_string (text));
}

QVariant
invoke_method (QObject *object, const char *method)
{
  QVariant value;
  QMetaObject::invokeMethod (object, method, Qt::DirectConnection,
                             Q_RETURN_ARG (QVariant, value));
  return value;
};
