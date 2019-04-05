/*
 * app.cpp
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

QString
scm_to_human (SCM in)
{
  SCM fmt = scm_call_2 (scm_c_public_ref ("guile", "format"),
                        scm_from_utf8_string ("~a"), in);
  return scm_to_qstring (fmt);
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
