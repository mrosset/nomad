#include "keymap.h"
#include <libguile.h>

Keymap::Keymap (QObject *parent) : QObject (parent) {}

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
