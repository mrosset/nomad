---
title: "Nomad: Shroud password manager integration"
date: 2019-06-18
draft: false
author: amar-m-singh
template: article.pug
---

Adding shroud password manager module `(nomad shroud)` to Nomad
browser.

## Usage

Here is how you would use this module:

``` {.scheme}
(use-modules (nomad shroud))
(set! shroud-database-file (~/ ".config/shroud/db.gpg"))
(set! shroud-config-file (~/ ".shroud"))
```

In Nomad M-x shroud-find-password str RET will add the password to
clipboard.

We define an interactive command using `define-command` macro.

``` {.scheme}
(define-command (shroud-find-password entry)
  "Show password/secrets entry"
  (clipboard-copy (shroud-show-entry (car (shroud-find-entries entry)) "password")))
```

But wait there is no function like `clipboard-copy` in Nomad, so we have
to define one.

``` {.c org-language="C"}
SCM_DEFINE_PUBLIC (scm_nomad_clipboard_copy, "clipboard-copy", 1, 0, 0, (SCM x),
                   "Copy X to clipboard.")
{
  GtkClipboard *clip = gtk_clipboard_get_default (gdk_display_get_default ());
  char *c_text = scm_to_locale_string (x);
  int len = scm_to_int (scm_string_length (x));

  scm_dynwind_begin (0);
  gtk_clipboard_set_text (clip, c_text, len);
  scm_dynwind_free (c_text);
  scm_dynwind_end ();
  return SCM_BOOL_T;
}
```

And that\'s it, you can check out the sources at
[Github](https://github.com/o-nly/nomad/commit/cb1d610870c378df51fab3d5cfc2923bdd32589c).
