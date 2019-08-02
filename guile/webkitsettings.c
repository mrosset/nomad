/*
 * webkit-settings --- configure webkit renderer.
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


SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_web_view_get_settings , "webkit_web_view_get_settings",
                   1, 0, 0, (SCM pointer),
                   "Get webkit-web-view settings from webkit-web-view pointer.")
{
  return scm_from_pointer
    (webkit_web_view_get_settings (scm_to_pointer (pointer)), NULL);
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_web_view_set_settings , "webkit_web_view_set_settings",
                   2, 0, 0, (SCM pointer, SCM settings),
                   "Set webkit-web-view settings for view pointer to settings pointer.")
{
  webkit_web_view_set_settings
    (scm_to_pointer (pointer), scm_to_pointer (settings));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_new , "webkit-settings-new",
                   0, 0, 0, (),
                   "Return a pointer to freshly created webkit-settings.")
{
  return scm_from_pointer (webkit_settings_new (), NULL);
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_auto_load_images , "webkit_settings_get_auto_load_images",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_auto_load_images from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_auto_load_images (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_auto_load_images , "webkit_settings_set_auto_load_images",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_auto_load_images for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_auto_load_images (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_frame_flattening , "webkit_settings_get_enable_frame_flattening",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_frame_flattening from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_frame_flattening (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_frame_flattening , "webkit_settings_set_enable_frame_flattening",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_frame_flattening for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_frame_flattening (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_html5_database , "webkit_settings_get_enable_html5_database",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_html5_database from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_html5_database (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_html5_database , "webkit_settings_set_enable_html5_database",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_html5_database for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_html5_database (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_html5_local_storage , "webkit_settings_get_enable_html5_local_storage",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_html5_local_storage from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_html5_local_storage (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_html5_local_storage , "webkit_settings_set_enable_html5_local_storage",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_html5_local_storage for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_html5_local_storage (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_hyperlink_auditing , "webkit_settings_get_enable_hyperlink_auditing",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_hyperlink_auditing from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_hyperlink_auditing (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_hyperlink_auditing , "webkit_settings_set_enable_hyperlink_auditing",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_hyperlink_auditing for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_hyperlink_auditing (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_java , "webkit_settings_get_enable_java",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_java from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_java (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_java , "webkit_settings_set_enable_java",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_java for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_java (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_javascript , "webkit_settings_get_enable_javascript",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_javascript from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_javascript (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_javascript , "webkit_settings_set_enable_javascript",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_javascript for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_javascript (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: compile time error. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_javascript_markup , "webkit_settings_get_enable_javascript_markup", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_javascript_markup from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_javascript_markup (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_javascript_markup , "webkit_settings_set_enable_javascript_markup", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_javascript_markup for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_javascript_markup (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_offline_web_application_cache , "webkit_settings_get_enable_offline_web_application_cache",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_offline_web_application_cache from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_offline_web_application_cache (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_offline_web_application_cache , "webkit_settings_set_enable_offline_web_application_cache",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_offline_web_application_cache for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_offline_web_application_cache (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_plugins , "webkit_settings_get_enable_plugins",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_plugins from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_plugins (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_plugins , "webkit_settings_set_enable_plugins",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_plugins for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_plugins (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_xss_auditor , "webkit_settings_get_enable_xss_auditor",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_xss_auditor from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_xss_auditor (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_xss_auditor , "webkit_settings_set_enable_xss_auditor",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_xss_auditor for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_xss_auditor (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_javascript_can_open_windows_automatically , "webkit_settings_get_javascript_can_open_windows_automatically",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_javascript_can_open_windows_automatically from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_javascript_can_open_windows_automatically (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_javascript_can_open_windows_automatically , "webkit_settings_set_javascript_can_open_windows_automatically",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_javascript_can_open_windows_automatically for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_javascript_can_open_windows_automatically (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_load_icons_ignoring_image_load_setting , "webkit_settings_get_load_icons_ignoring_image_load_setting",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_load_icons_ignoring_image_load_setting from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_load_icons_ignoring_image_load_setting (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_load_icons_ignoring_image_load_setting , "webkit_settings_set_load_icons_ignoring_image_load_setting",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_load_icons_ignoring_image_load_setting for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_load_icons_ignoring_image_load_setting (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_font_family , "webkit_settings_get_default_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_default_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_font_family , "webkit_settings_set_default_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_monospace_font_family , "webkit_settings_get_monospace_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_monospace_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_monospace_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_monospace_font_family , "webkit_settings_set_monospace_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_monospace_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_monospace_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_serif_font_family , "webkit_settings_get_serif_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_serif_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_serif_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_serif_font_family , "webkit_settings_set_serif_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_serif_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_serif_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_sans_serif_font_family , "webkit_settings_get_sans_serif_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_sans_serif_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_sans_serif_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_sans_serif_font_family , "webkit_settings_set_sans_serif_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_sans_serif_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_sans_serif_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_cursive_font_family , "webkit_settings_get_cursive_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_cursive_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_cursive_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_cursive_font_family , "webkit_settings_set_cursive_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_cursive_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_cursive_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_fantasy_font_family , "webkit_settings_get_fantasy_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_fantasy_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_fantasy_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_fantasy_font_family , "webkit_settings_set_fantasy_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_fantasy_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_fantasy_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_pictograph_font_family , "webkit_settings_get_pictograph_font_family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_pictograph_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_pictograph_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_pictograph_font_family , "webkit_settings_set_pictograph_font_family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_pictograph_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_pictograph_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_font_size , "webkit_settings_get_default_font_size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_font_size from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_default_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_font_size , "webkit_settings_set_default_font_size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_font_size (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_monospace_font_size , "webkit_settings_get_default_monospace_font_size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_monospace_font_size from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_default_monospace_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_monospace_font_size , "webkit_settings_set_default_monospace_font_size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_monospace_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_monospace_font_size (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_minimum_font_size , "webkit_settings_get_minimum_font_size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_minimum_font_size from webkitsettings (pointer).")
{
  return scm_from_int (webkit_settings_get_minimum_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_minimum_font_size , "webkit_settings_set_minimum_font_size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_minimum_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_minimum_font_size (scm_to_pointer (pointer), scm_to_int (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_charset , "webkit_settings_get_default_charset",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_charset from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_default_charset (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_charset , "webkit_settings_set_default_charset",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_charset for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_charset (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: deprecated. has to be implemented some other way. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_private_browsing , "webkit_settings_get_enable_private_browsing", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_private_browsing from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_private_browsing (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_private_browsing , "webkit_settings_set_enable_private_browsing", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_private_browsing for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_private_browsing (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_developer_extras , "webkit_settings_get_enable_developer_extras",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_developer_extras from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_developer_extras (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_developer_extras , "webkit_settings_set_enable_developer_extras",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_developer_extras for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_developer_extras (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_resizable_text_areas , "webkit_settings_get_enable_resizable_text_areas",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_resizable_text_areas from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_resizable_text_areas (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_resizable_text_areas , "webkit_settings_set_enable_resizable_text_areas",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_resizable_text_areas for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_resizable_text_areas (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_tabs_to_links , "webkit_settings_get_enable_tabs_to_links",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_tabs_to_links from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_tabs_to_links (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_tabs_to_links , "webkit_settings_set_enable_tabs_to_links",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_tabs_to_links for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_tabs_to_links (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_dns_prefetching , "webkit_settings_get_enable_dns_prefetching",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_dns_prefetching from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_dns_prefetching (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_dns_prefetching , "webkit_settings_set_enable_dns_prefetching",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_dns_prefetching for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_dns_prefetching (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_caret_browsing , "webkit_settings_get_enable_caret_browsing",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_caret_browsing from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_caret_browsing (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_caret_browsing , "webkit_settings_set_enable_caret_browsing",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_caret_browsing for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_caret_browsing (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_fullscreen , "webkit_settings_get_enable_fullscreen",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_fullscreen from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_fullscreen (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_fullscreen , "webkit_settings_set_enable_fullscreen",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_fullscreen for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_fullscreen (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_print_backgrounds , "webkit_settings_get_print_backgrounds",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_print_backgrounds from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_print_backgrounds (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_print_backgrounds , "webkit_settings_set_print_backgrounds",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_print_backgrounds for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_print_backgrounds (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_webaudio , "webkit_settings_get_enable_webaudio",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_webaudio from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_webaudio (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_webaudio , "webkit_settings_set_enable_webaudio",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_webaudio for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_webaudio (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_webgl , "webkit_settings_get_enable_webgl",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_webgl from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_webgl (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_webgl , "webkit_settings_set_enable_webgl",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_webgl for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_webgl (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_modal_dialogs , "webkit_settings_get_allow_modal_dialogs",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_modal_dialogs from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_modal_dialogs (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_modal_dialogs , "webkit_settings_set_allow_modal_dialogs",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_modal_dialogs for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_modal_dialogs (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_zoom_text_only , "webkit_settings_get_zoom_text_only",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_zoom_text_only from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_zoom_text_only (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_zoom_text_only , "webkit_settings_set_zoom_text_only",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_zoom_text_only for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_zoom_text_only (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_javascript_can_access_clipboard , "webkit_settings_get_javascript_can_access_clipboard",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_javascript_can_access_clipboard from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_javascript_can_access_clipboard (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_javascript_can_access_clipboard , "webkit_settings_set_javascript_can_access_clipboard",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_javascript_can_access_clipboard for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_javascript_can_access_clipboard (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_media_playback_requires_user_gesture , "webkit_settings_get_media_playback_requires_user_gesture",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_media_playback_requires_user_gesture from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_media_playback_requires_user_gesture (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_media_playback_requires_user_gesture , "webkit_settings_set_media_playback_requires_user_gesture",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_media_playback_requires_user_gesture for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_media_playback_requires_user_gesture (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_media_playback_allows_inline , "webkit_settings_get_media_playback_allows_inline",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_media_playback_allows_inline from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_media_playback_allows_inline (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_media_playback_allows_inline , "webkit_settings_set_media_playback_allows_inline",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_media_playback_allows_inline for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_media_playback_allows_inline (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_draw_compositing_indicators , "webkit_settings_get_draw_compositing_indicators",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_draw_compositing_indicators from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_draw_compositing_indicators (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_draw_compositing_indicators , "webkit_settings_set_draw_compositing_indicators",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_draw_compositing_indicators for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_draw_compositing_indicators (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_site_specific_quirks , "webkit_settings_get_enable_site_specific_quirks",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_site_specific_quirks from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_site_specific_quirks (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_site_specific_quirks , "webkit_settings_set_enable_site_specific_quirks",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_site_specific_quirks for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_site_specific_quirks (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_page_cache , "webkit_settings_get_enable_page_cache",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_page_cache from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_page_cache (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_page_cache , "webkit_settings_set_enable_page_cache",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_page_cache for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_page_cache (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_user_agent , "webkit_settings_get_user_agent",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_user_agent from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_user_agent (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_user_agent , "webkit_settings_set_user_agent",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_user_agent for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_user_agent (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: This is maybe not useful for Nomad. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_user_agent_with_application_details , "webkit_settings_set_user_agent_with_application_details", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_user_agent_with_application_details for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_user_agent_with_application_details (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */


SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_smooth_scrolling , "webkit_settings_get_enable_smooth_scrolling",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_smooth_scrolling from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_smooth_scrolling (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_smooth_scrolling , "webkit_settings_set_enable_smooth_scrolling",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_smooth_scrolling for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_smooth_scrolling (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_accelerated_2d_canvas , "webkit_settings_get_enable_accelerated_2d_canvas",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_accelerated_2d_canvas from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_accelerated_2d_canvas (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_accelerated_2d_canvas , "webkit_settings_set_enable_accelerated_2d_canvas",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_accelerated_2d_canvas for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_accelerated_2d_canvas (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_write_console_messages_to_stdout , "webkit_settings_get_enable_write_console_messages_to_stdout",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_write_console_messages_to_stdout from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_write_console_messages_to_stdout (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_write_console_messages_to_stdout , "webkit_settings_set_enable_write_console_messages_to_stdout",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_write_console_messages_to_stdout for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_write_console_messages_to_stdout (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_media_stream , "webkit_settings_get_enable_media_stream",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_media_stream from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_media_stream (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_media_stream , "webkit_settings_set_enable_media_stream",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_media_stream for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_media_stream (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: compile error */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_mock_capture_devices , "webkit_settings_get_enable_mock_capture_devices", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_mock_capture_devices from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_mock_capture_devices (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_mock_capture_devices , "webkit_settings_set_enable_mock_capture_devices", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_mock_capture_devices for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_mock_capture_devices (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_spatial_navigation , "webkit_settings_get_enable_spatial_navigation",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_spatial_navigation from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_spatial_navigation (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_spatial_navigation , "webkit_settings_set_enable_spatial_navigation",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_spatial_navigation for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_spatial_navigation (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_mediasource , "webkit_settings_get_enable_mediasource",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_mediasource from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_mediasource (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_mediasource , "webkit_settings_set_enable_mediasource",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_mediasource for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_mediasource (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_encrypted_media , "webkit_settings_get_enable_encrypted_media",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_encrypted_media from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_encrypted_media (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_encrypted_media , "webkit_settings_set_enable_encrypted_media",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_encrypted_media for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_encrypted_media (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

/* FIXME: compile error */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_media_capabilities , "webkit_settings_get_enable_media_capabilities", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_media_capabilities from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_media_capabilities (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_media_capabilities , "webkit_settings_set_enable_media_capabilities", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_media_capabilities for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_media_capabilities (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_file_access_from_file_urls , "webkit_settings_get_allow_file_access_from_file_urls",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_file_access_from_file_urls from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_file_access_from_file_urls (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_file_access_from_file_urls , "webkit_settings_set_allow_file_access_from_file_urls",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_file_access_from_file_urls for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_file_access_from_file_urls (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_universal_access_from_file_urls , "webkit_settings_get_allow_universal_access_from_file_urls",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_universal_access_from_file_urls from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_universal_access_from_file_urls (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_universal_access_from_file_urls , "webkit_settings_set_allow_universal_access_from_file_urls",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_universal_access_from_file_urls for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_universal_access_from_file_urls (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

/*  FIXME: needs more work and thought.*/
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_hardware_acceleration_policy , "webkit_settings_get_hardware_acceleration_policy", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_hardware_acceleration_policy from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_hardware_acceleration_policy (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_hardware_acceleration_policy , "webkit_settings_set_hardware_acceleration_policy", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_hardware_acceleration_policy for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_hardware_acceleration_policy (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

/* FIXME: compile error. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_back_forward_navigation_gestures , "webkit_settings_get_enable_back_forward_navigation_gestures", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_back_forward_navigation_gestures from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_back_forward_navigation_gestures (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_back_forward_navigation_gestures , "webkit_settings_set_enable_back_forward_navigation_gestures", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_back_forward_navigation_gestures for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_back_forward_navigation_gestures (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

/* these do not belong here anyway. They are webkit provided utils for data type conversions. */
/* webkit_settings_font_size_to_points */
/* webkit_settings_font_size_to_pixels */


void
nomad_webkitsettings_register_function (void *data)
{
#ifndef SCM_MAGIC_SNARFER
#include "webkitsettings.x"
#endif
}
