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
                   scm_nomad_webkit_web_view_get_settings , "webkit-web-view-get-settings",
                   1, 0, 0, (SCM pointer),
                   "Get webkit-web-view settings from webkit-web-view pointer.")
{
  return scm_from_pointer
    (webkit_web_view_get_settings (scm_to_pointer (pointer)), NULL);
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_web_view_set_settings , "webkit-web-view-set-settings",
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
                   scm_nomad_webkit_settings_get_auto_load_images , "webkit-settings-get-auto-load-images",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_auto_load_images from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_auto_load_images (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_auto_load_images , "webkit-settings-set-auto-load-images",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_auto_load_images for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_auto_load_images (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_frame_flattening , "webkit-settings-get-enable-frame-flattening",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_frame_flattening from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_frame_flattening (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_frame_flattening , "webkit-settings-set-enable-frame-flattening",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_frame_flattening for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_frame_flattening (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_html5_database , "webkit-settings-get-enable-html5-database",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_html5_database from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_html5_database (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_html5_database , "webkit-settings-set-enable-html5-database",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_html5_database for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_html5_database (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_html5_local_storage , "webkit-settings-get-enable-html5-local-storage",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_html5_local_storage from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_html5_local_storage (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_html5_local_storage , "webkit-settings-set-enable-html5-local-storage",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_html5_local_storage for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_html5_local_storage (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_hyperlink_auditing , "webkit-settings-get-enable-hyperlink-auditing",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_hyperlink_auditing from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_hyperlink_auditing (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_hyperlink_auditing , "webkit-settings-set-enable-hyperlink-auditing",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_hyperlink_auditing for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_hyperlink_auditing (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_java , "webkit-settings-get-enable-java",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_java from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_java (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_java , "webkit-settings-set-enable-java",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_java for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_java (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_javascript , "webkit-settings-get-enable-javascript",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_javascript from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_javascript (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_javascript , "webkit-settings-set-enable-javascript",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_javascript for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_javascript (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: compile time error. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_javascript_markup , "webkit-settings-get-enable-javascript-markup", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_javascript_markup from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_javascript_markup (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_javascript_markup , "webkit-settings-set-enable-javascript-markup", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_javascript_markup for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_javascript_markup (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_offline_web_application_cache , "webkit-settings-get-enable-offline-web-application-cache",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_offline_web_application_cache from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_offline_web_application_cache (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_offline_web_application_cache , "webkit-settings-set-enable-offline-web-application-cache",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_offline_web_application_cache for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_offline_web_application_cache (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_plugins , "webkit-settings-get-enable-plugins",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_plugins from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_plugins (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_plugins , "webkit-settings-set-enable-plugins",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_plugins for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_plugins (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_xss_auditor , "webkit-settings-get-enable-xss-auditor",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_xss_auditor from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_xss_auditor (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_xss_auditor , "webkit-settings-set-enable-xss-auditor",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_xss_auditor for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_xss_auditor (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_javascript_can_open_windows_automatically , "webkit-settings-get-javascript-can-open-windows-automatically",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_javascript_can_open_windows_automatically from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_javascript_can_open_windows_automatically (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_javascript_can_open_windows_automatically , "webkit-settings-set-javascript-can-open-windows-automatically",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_javascript_can_open_windows_automatically for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_javascript_can_open_windows_automatically (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_load_icons_ignoring_image_load_setting , "webkit-settings-get-load-icons-ignoring-image-load-setting",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_load_icons_ignoring_image_load_setting from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_load_icons_ignoring_image_load_setting (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_load_icons_ignoring_image_load_setting , "webkit-settings-set-load-icons-ignoring-image-load-setting",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_load_icons_ignoring_image_load_setting for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_load_icons_ignoring_image_load_setting (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_font_family , "webkit-settings-get-default-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_default_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_font_family , "webkit-settings-set-default-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_monospace_font_family , "webkit-settings-get-monospace-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_monospace_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_monospace_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_monospace_font_family , "webkit-settings-set-monospace-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_monospace_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_monospace_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_serif_font_family , "webkit-settings-get-serif-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_serif_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_serif_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_serif_font_family , "webkit-settings-set-serif-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_serif_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_serif_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_sans_serif_font_family , "webkit-settings-get-sans-serif-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_sans_serif_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_sans_serif_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_sans_serif_font_family , "webkit-settings-set-sans-serif-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_sans_serif_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_sans_serif_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_cursive_font_family , "webkit-settings-get-cursive-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_cursive_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_cursive_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_cursive_font_family , "webkit-settings-set-cursive-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_cursive_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_cursive_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_fantasy_font_family , "webkit-settings-get-fantasy-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_fantasy_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_fantasy_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_fantasy_font_family , "webkit-settings-set-fantasy-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_fantasy_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_fantasy_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_pictograph_font_family , "webkit-settings-get-pictograph-font-family",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_pictograph_font_family from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_pictograph_font_family (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_pictograph_font_family , "webkit-settings-set-pictograph-font-family",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_pictograph_font_family for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_pictograph_font_family (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_font_size , "webkit-settings-get-default-font-size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_font_size from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_default_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_font_size , "webkit-settings-set-default-font-size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_font_size (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_monospace_font_size , "webkit-settings-get-default-monospace-font-size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_monospace_font_size from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_default_monospace_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_monospace_font_size , "webkit-settings-set-default-monospace-font-size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_monospace_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_monospace_font_size (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_minimum_font_size , "webkit-settings-get-minimum-font-size",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_minimum_font_size from webkitsettings (pointer).")
{
  return scm_from_int (webkit_settings_get_minimum_font_size (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_minimum_font_size , "webkit-settings-set-minimum-font-size",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_minimum_font_size for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_minimum_font_size (scm_to_pointer (pointer), scm_to_int (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_default_charset , "webkit-settings-get-default-charset",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_default_charset from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_default_charset (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_default_charset , "webkit-settings-set-default-charset",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_default_charset for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_default_charset (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: deprecated. has to be implemented some other way. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_private_browsing , "webkit-settings-get-enable-private-browsing", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_private_browsing from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_private_browsing (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_private_browsing , "webkit-settings-set-enable-private-browsing", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_private_browsing for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_private_browsing (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_developer_extras , "webkit-settings-get-enable-developer-extras",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_developer_extras from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_developer_extras (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_developer_extras , "webkit-settings-set-enable-developer-extras",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_developer_extras for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_developer_extras (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_resizable_text_areas , "webkit-settings-get-enable-resizable-text-areas",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_resizable_text_areas from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_resizable_text_areas (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_resizable_text_areas , "webkit-settings-set-enable-resizable-text-areas",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_resizable_text_areas for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_resizable_text_areas (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_tabs_to_links , "webkit-settings-get-enable-tabs-to-links",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_tabs_to_links from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_tabs_to_links (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_tabs_to_links , "webkit-settings-set-enable-tabs-to-links",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_tabs_to_links for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_tabs_to_links (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_dns_prefetching , "webkit-settings-get-enable-dns-prefetching",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_dns_prefetching from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_dns_prefetching (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_dns_prefetching , "webkit-settings-set-enable-dns-prefetching",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_dns_prefetching for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_dns_prefetching (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_caret_browsing , "webkit-settings-get-enable-caret-browsing",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_caret_browsing from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_caret_browsing (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_caret_browsing , "webkit-settings-set-enable-caret-browsing",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_caret_browsing for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_caret_browsing (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_fullscreen , "webkit-settings-get-enable-fullscreen",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_fullscreen from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_fullscreen (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_fullscreen , "webkit-settings-set-enable-fullscreen",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_fullscreen for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_fullscreen (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_print_backgrounds , "webkit-settings-get-print-backgrounds",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_print_backgrounds from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_print_backgrounds (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_print_backgrounds , "webkit-settings-set-print-backgrounds",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_print_backgrounds for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_print_backgrounds (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_webaudio , "webkit-settings-get-enable-webaudio",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_webaudio from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_webaudio (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_webaudio , "webkit-settings-set-enable-webaudio",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_webaudio for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_webaudio (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_webgl , "webkit-settings-get-enable-webgl",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_webgl from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_webgl (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_webgl , "webkit-settings-set-enable-webgl",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_webgl for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_webgl (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_modal_dialogs , "webkit-settings-get-allow-modal-dialogs",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_modal_dialogs from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_modal_dialogs (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_modal_dialogs , "webkit-settings-set-allow-modal-dialogs",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_modal_dialogs for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_modal_dialogs (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_zoom_text_only , "webkit-settings-get-zoom-text-only",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_zoom_text_only from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_zoom_text_only (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_zoom_text_only , "webkit-settings-set-zoom-text-only",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_zoom_text_only for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_zoom_text_only (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_javascript_can_access_clipboard , "webkit-settings-get-javascript-can-access-clipboard",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_javascript_can_access_clipboard from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_javascript_can_access_clipboard (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_javascript_can_access_clipboard , "webkit-settings-set-javascript-can-access-clipboard",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_javascript_can_access_clipboard for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_javascript_can_access_clipboard (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_media_playback_requires_user_gesture , "webkit-settings-get-media-playback-requires-user-gesture",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_media_playback_requires_user_gesture from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_media_playback_requires_user_gesture (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_media_playback_requires_user_gesture , "webkit-settings-set-media-playback-requires-user-gesture",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_media_playback_requires_user_gesture for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_media_playback_requires_user_gesture (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_media_playback_allows_inline , "webkit-settings-get-media-playback-allows-inline",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_media_playback_allows_inline from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_media_playback_allows_inline (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_media_playback_allows_inline , "webkit-settings-set-media-playback-allows-inline",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_media_playback_allows_inline for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_media_playback_allows_inline (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_draw_compositing_indicators , "webkit-settings-get-draw-compositing-indicators",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_draw_compositing_indicators from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_draw_compositing_indicators (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_draw_compositing_indicators , "webkit-settings-set-draw-compositing-indicators",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_draw_compositing_indicators for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_draw_compositing_indicators (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_site_specific_quirks , "webkit-settings-get-enable-site-specific-quirks",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_site_specific_quirks from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_site_specific_quirks (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_site_specific_quirks , "webkit-settings-set-enable-site-specific-quirks",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_site_specific_quirks for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_site_specific_quirks (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_page_cache , "webkit-settings-get-enable-page-cache",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_page_cache from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_page_cache (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_page_cache , "webkit-settings-set-enable-page-cache",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_page_cache for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_page_cache (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_user_agent , "webkit-settings-get-user-agent",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_user_agent from webkitsettings (pointer).")
{
  return scm_from_locale_string (webkit_settings_get_user_agent (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_user_agent , "webkit-settings-set-user-agent",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_user_agent for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_user_agent (scm_to_pointer (pointer), scm_to_locale_string (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: This is maybe not useful for Nomad. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_user_agent_with_application_details , "webkit-settings-set-user-agent-with-application-details", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_user_agent_with_application_details for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_user_agent_with_application_details (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */


SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_smooth_scrolling , "webkit-settings-get-enable-smooth-scrolling",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_smooth_scrolling from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_smooth_scrolling (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_smooth_scrolling , "webkit-settings-set-enable-smooth-scrolling",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_smooth_scrolling for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_smooth_scrolling (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_accelerated_2d_canvas , "webkit-settings-get-enable-accelerated-2d-canvas",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_accelerated_2d_canvas from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_accelerated_2d_canvas (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_accelerated_2d_canvas , "webkit-settings-set-enable-accelerated-2d-canvas",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_accelerated_2d_canvas for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_accelerated_2d_canvas (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_write_console_messages_to_stdout , "webkit-settings-get-enable-write-console-messages-to-stdout",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_write_console_messages_to_stdout from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_write_console_messages_to_stdout (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_write_console_messages_to_stdout , "webkit-settings-set-enable-write-console-messages-to-stdout",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_write_console_messages_to_stdout for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_write_console_messages_to_stdout (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_media_stream , "webkit-settings-get-enable-media-stream",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_media_stream from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_media_stream (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_media_stream , "webkit-settings-set-enable-media-stream",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_media_stream for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_media_stream (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}
/* FIXME: compile error */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_mock_capture_devices , "webkit-settings-get-enable-mock-capture-devices", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_mock_capture_devices from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_mock_capture_devices (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_mock_capture_devices , "webkit-settings-set-enable-mock-capture-devices", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_mock_capture_devices for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_mock_capture_devices (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_spatial_navigation , "webkit-settings-get-enable-spatial-navigation",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_spatial_navigation from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_spatial_navigation (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_spatial_navigation , "webkit-settings-set-enable-spatial-navigation",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_spatial_navigation for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_spatial_navigation (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_mediasource , "webkit-settings-get-enable-mediasource",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_mediasource from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_mediasource (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_mediasource , "webkit-settings-set-enable-mediasource",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_mediasource for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_mediasource (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_enable_encrypted_media , "webkit-settings-get-enable-encrypted-media",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_enable_encrypted_media from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_enable_encrypted_media (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_enable_encrypted_media , "webkit-settings-set-enable-encrypted-media",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_enable_encrypted_media for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_enable_encrypted_media (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

/* FIXME: compile error */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_media_capabilities , "webkit-settings-get-enable-media-capabilities", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_media_capabilities from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_media_capabilities (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_media_capabilities , "webkit-settings-set-enable-media-capabilities", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_enable_media_capabilities for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_enable_media_capabilities (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_file_access_from_file_urls , "webkit-settings-get-allow-file-access-from-file-urls",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_file_access_from_file_urls from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_file_access_from_file_urls (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_file_access_from_file_urls , "webkit-settings-set-allow-file-access-from-file-urls",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_file_access_from_file_urls for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_file_access_from_file_urls (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_get_allow_universal_access_from_file_urls , "webkit-settings-get-allow-universal-access-from-file-urls",
                   1, 0, 0, (SCM pointer),
                   "Get webkit_settings_get_allow_universal_access_from_file_urls from webkitsettings (pointer).")
{
  return scm_from_bool (webkit_settings_get_allow_universal_access_from_file_urls (scm_to_pointer (pointer)));
}

SCM_DEFINE_PUBLIC (
                   scm_nomad_webkit_settings_set_allow_universal_access_from_file_urls , "webkit-settings-set-allow-universal-access-from-file-urls",
                   2, 0, 0, (SCM pointer, SCM enabled),
                   "Set webkit_settings_set_allow_universal_access_from_file_urls for webkitsettings (pointer) to boolean (enabled).")
{
  webkit_settings_set_allow_universal_access_from_file_urls (scm_to_pointer (pointer), scm_to_bool (enabled));
  return SCM_UNSPECIFIED;
}

/*  FIXME: needs more work and thought.*/
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_hardware_acceleration_policy , "webkit-settings-get-hardware-acceleration-policy", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_hardware_acceleration_policy from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_hardware_acceleration_policy (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_hardware_acceleration_policy , "webkit-settings-set-hardware-acceleration-policy", */
/*                    2, 0, 0, (SCM pointer, SCM enabled), */
/*                    "Set webkit_settings_set_hardware_acceleration_policy for webkitsettings (pointer) to boolean (enabled).") */
/* { */
/*   webkit_settings_set_hardware_acceleration_policy (scm_to_pointer (pointer), scm_to_bool (enabled)); */
/*   return SCM_UNSPECIFIED; */
/* } */

/* FIXME: compile error. */
/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_get_enable_back_forward_navigation_gestures , "webkit-settings-get-enable-back-forward-navigation-gestures", */
/*                    1, 0, 0, (SCM pointer), */
/*                    "Get webkit_settings_get_enable_back_forward_navigation_gestures from webkitsettings (pointer).") */
/* { */
/*   return scm_from_bool (webkit_settings_get_enable_back_forward_navigation_gestures (scm_to_pointer (pointer))); */
/* } */

/* SCM_DEFINE_PUBLIC ( */
/*                    scm_nomad_webkit_settings_set_enable_back_forward_navigation_gestures , "webkit-settings-set-enable-back-forward-navigation-gestures", */
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
