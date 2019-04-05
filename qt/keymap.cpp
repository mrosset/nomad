#include "keymap.h"
#include "app.h"

#include <QUrl>
#include <libguile.h>

Keymap::Keymap (QObject *parent) : QObject (parent) {}

static SCM
call_proc (void *data)
{
  SCM proc = (SCM) (data);

  // if obj is a procedure then it has not arguments, just call it directly
  if (scm_procedure_p (proc))
    {
      SCM result = scm_call_0 (proc);
      return scm_call_3 (scm_c_public_ref ("guile", "format"), SCM_BOOL_F,
                         scm_from_utf8_string ("~s"), result);
    }
  return SCM_UNSPECIFIED;
}

static SCM
call_proc_args (void *data)
{

  SCM obj = (SCM) (data);
  if (scm_list_p (obj))
    {
      SCM proc = scm_car (obj);
      SCM arg = scm_car (scm_cdr (obj));
      scm_call_1 (proc, arg);
    }
  return SCM_UNSPECIFIED;
}

static SCM
eval_handler (void *data, SCM key, SCM args)
{
  qInfo () << "key" << scm_to_human (key) << "param" << scm_to_human (args);
  return scm_call_4 (scm_c_public_ref ("guile", "format"), SCM_BOOL_F,
                     scm_from_utf8_string ("~s: ~s"), key, args);
}

void
Keymap::Eval (QString command)
{
  SCM key = scm_string_to_symbol (qstring_to_scm (command));
  SCM args = scm_call_1 (scm_c_public_ref ("nomad eval", "command-args"), key);
  SCM proc = scm_call_1 (scm_c_public_ref ("nomad eval", "command-ref"), key);

  // If procedure does not take any arguments. Call the procedure and
  // set the minibuffer to either value returned from the procedure or
  // the exception
  if (scm_is_null (args))
    {
      SCM result = scm_c_catch (SCM_BOOL_T, call_proc, proc, eval_handler,
                                NULL, NULL, NULL);
      emit setMiniBuffer (QVariant (scm_to_human (result)));
      return;
    }

  // If procedure has arguments then have the minibuffer prompt for
  // arguements
  emit promptInput (
      QVariant (command),
      QVariant (scm_to_qstring (scm_symbol_to_string (scm_car (args)))));
}

void
Keymap::EvalWithArgs (QString command, QString arg0)
{
  SCM symbol = scm_string_to_symbol (qstring_to_scm (command));
  SCM lst = scm_variable_ref ((scm_c_lookup ("command-alist")));
  SCM pargs = scm_list_2 (scm_assoc_ref (lst, symbol), qstring_to_scm (arg0));

  SCM result = scm_c_catch (SCM_BOOL_T, call_proc_args, pargs, eval_handler,
                            NULL, NULL, NULL);
  emit setMiniBuffer (QVariant (scm_to_human (result)));
}

void
Keymap::historyComplete (QString input)
{
  SCM text = qstring_to_scm (input);
  SCM found = scm_call_1 (
      scm_c_public_ref ("nomad minibuffer", "history-completion"), text);

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
Keymap::Complete (QString input)
{
  SCM text = qstring_to_scm (input);
  SCM found = scm_call_1 (
      scm_c_public_ref ("nomad minibuffer", "input-completion"), text);

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
  scm_call_0 (scm_c_public_ref ("nomad init", "shutdown"));
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
Keymap::handleMessage (QString msg)
{
  emit setMiniBuffer (QVariant (msg));
}
