---
title: "Adding Bookmarks to Nomad"
date: 2019-07-02
draft: false
author: amar-m-singh
template: article.pug
---

Adding a simple bookmarks module `(nomad bookmark)` to Nomad browser.

## Usage

Here is how you would use this module:

``` {.scheme}
(use-modules (nomad bookmark))
(set! bookmark-file (~/ ".nomad.d/bookmarks.scm"))
(bookmark-init)
```

In Nomad:

-   M-x open-bookmark \<regex\> RET open the bookmark in a new buffer.
-   M-x save-bookmark id RET will save the current page as a bookmark.

A lot of the functioning of bookmarks is similar to how shroud works.
For example just like shroud a bookmark is a Scheme `record-type` with
two fields `id` and `contents` accessible by `(bookmark-id <bookmark>)`
and `(bookmark-id <bookmark>)` respectively. The idea is perhaps we can
have tags like functionality without requiring too much addititional
work.

Procedures are provided for conversion between `alist` and `<bookmark>`
records.

Patch is at
[Github](https://github.com/o-nly/nomad/commit/4dd95b229ee03b9ec85ab93a44e1138e3eca5ee0).
