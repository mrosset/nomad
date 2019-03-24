#include "keymap.h"
#include <libguile.h>

Keymap::Keymap (QObject *parent) : QObject (parent) {}

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

QVariant
Keymap::GetBuffer (QVariant index)
{
  return emit getBuffer (index);
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
Keymap::handleKeymap (int modifiers, int key)
{
  scm_run_hook (scm_c_public_ref ("nomad keymap", "key-press-hook"),
                scm_list_2 (scm_from_int (modifiers), scm_from_int (key)));
}
