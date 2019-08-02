/*
 * webkit-proxy --- configure webkit network settings.
 *
 * Copyright (C) 2019 Amar Singh<nly@disroot.org>
 *
 * This file is part of Nomad.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "webkit.h"
#include "util.h"
#include "app.h"
#include <libguile.h>
#include <webkit2/webkit2.h>

SCM_DEFINE_PUBLIC (scm_nomad_webkit_network_proxy_settings_new,
                   "webkit-proxy-settings-new", 2, 0, 0,
                   (SCM proxy, SCM ignore),
                   "Returns a newly initialized webkit proxy settings.")
{
  return scm_from_pointer (
      webkit_network_proxy_settings_new (
          scm_to_locale_string (proxy),
          scm_to_pointer (scm_nomad_list_to_argv (ignore))),
      NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_network_proxy_settings_free,
                   "webkit-proxy-settings-free", 1, 0, 0, (SCM pointer),
                   "Frees up a webkit proxy settings.")
{
  webkit_network_proxy_settings_free (scm_to_pointer (pointer));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_network_proxy_settings_copy,
                   "webkit-proxy-settings-copy", 1, 0, 0, (SCM pointer),
                   "Copies a webkit proxy settings.")
{
  return scm_from_pointer (
      webkit_network_proxy_settings_copy (scm_to_pointer (pointer)), NULL);
}

SCM_DEFINE_PUBLIC (scm_nomad_webkit_network_proxy_add_proxy_for_scheme,
                   "webkit-proxy-settings-add-proxy-for-scheme", 3, 0, 0,
                   (SCM pointer, SCM scheme, SCM proxy),
                   "Adds a URI-scheme-specific proxy. URIs whose scheme "
                   "matches scheme will be proxied via proxy.")
{
  webkit_network_proxy_settings_add_proxy_for_scheme (
      scm_to_pointer (pointer), scm_to_locale_string (scheme),
      scm_to_locale_string (scheme));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
    scm_nomad_webkit_web_context_set_network_proxy_settings_custom,
    "webkit-set-proxy-settings-custom", 1, 0, 0, (SCM pointer),
    "Activate custom proxy pointer points to.")
{
  webkit_web_context_set_network_proxy_settings (
      webkit_web_context_get_default (), WEBKIT_NETWORK_PROXY_MODE_CUSTOM,
      scm_to_pointer (pointer));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
    scm_nomad_webkit_web_context_set_network_proxy_settings_default,
    "webkit-set-proxy-settings-default", 0, 0, 0, (),
    "Activate system default proxy.")
{
  webkit_web_context_set_network_proxy_settings (
      webkit_web_context_get_default (), WEBKIT_NETWORK_PROXY_MODE_DEFAULT,
      NULL);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
    scm_nomad_webkit_web_context_set_network_proxy_settings_no_proxy,
    "webkit-set-proxy-settings-no-proxy", 0, 0, 0, (),
    "Activate system default proxy.")
{
  webkit_web_context_set_network_proxy_settings (
      webkit_web_context_get_default (), WEBKIT_NETWORK_PROXY_MODE_NO_PROXY,
      NULL);
  return SCM_UNSPECIFIED;
}

void
nomad_webkitproxy_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkitproxy.x"
#endif
}
