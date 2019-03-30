#include "keymap.h"
#include "app.h"

#include <QUrl>
#include <libguile.h>

Keymap::Keymap (QObject *parent) : QObject (parent) {}

void
Keymap::Eval (QString input)
{
  qInfo () << "eval:" << input;
  input = input.prepend ("(").append (")");
  SCM exp = qstring_to_scm (input);
  SCM values = scm_call_1 (scm_c_public_ref ("nomad util", "input-eval"), exp);
  SCM result = scm_c_value_ref (values, 0);
  SCM err = scm_c_value_ref (values, 1);

  // if value err is not #nil then return the exception as a string
  // and return
  if (!scm_is_null (err))
    {
      emit setMiniBuffer (QVariant (scm_to_utf8_string (err)));
      return;
    }
  emit setMiniBuffer (QVariant (scm_to_utf8_string (result)));
}

void
Keymap::Complete (QString input)
{
  SCM text;
  SCM found;

  text = qstring_to_scm (input);
  found
      = scm_call_1 (scm_c_public_ref ("nomad util", "input-completion"), text);

  emit clearMiniOutput ();
  // return if nothing found
  if (scm_not (found) == SCM_BOOL_T)
    {
      return;
    }

  QList<QString> ql;
  for (int i = 0; i < scm_to_int (scm_length (found)); i++)
    {
      QString qs = scm_to_qstring (scm_list_ref (found, scm_from_int (i)));
      ql.append (qs);
    }
  emit setMiniOutput (QVariant (ql));
}

void
Keymap::Kill ()
{
  scm_call_1 (scm_c_public_ref ("nomad repl", "server-force-delete"),
              scm_c_eval_string ("(option-listen (command-line))"));
}

void
Keymap::MakeFrame (QVariant uri)
{
  emit makeFrame (uri);
}

void
Keymap::MakeBuffer (QVariant uri)
{
  emit makeBuffer (uri);
}

SCM
Keymap::GetBuffer (QVariant index)
{
  QVariant var = getBuffer (index);
  qInfo () << var;
  // QUrl uri = qvariant_cast<QUrl> (var);
  // const char *url = uri.toString ().toUtf8 ().constData ();
  return scm_from_utf8_string ("foo");
}

void
Keymap::NextBuffer ()
{
  emit nextBuffer ();
}

void
Keymap::handleScrollv (QVariant offset)
{
  emit scrollv (offset);
}

void
Keymap::handleGoForward ()
{
  emit goForward ();
}

void
Keymap::handleGoBack ()
{
  emit goBack ();
}

void
Keymap::handleKillBuffer ()
{
  emit killBuffer ();
}

void
Keymap::handleKeymap (QString keymap, int modifiers, int key)
{
  SCM km = scm_variable_ref (scm_c_lookup (keymap.toUtf8 ()));
  scm_run_hook (scm_c_public_ref ("nomad keymap", "key-press-hook"),
                scm_list_3 (km, scm_from_int (modifiers), scm_from_int (key)));
}

void
Keymap::SetUrl (QVariant url)
{
  emit setUrl (url);
}

void
Keymap::handleMiniBufferSelect (QVariant offset)
{
  emit miniBufferSelect (offset);
}
