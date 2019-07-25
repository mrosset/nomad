/*
 * buffer.c
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

#include <webkit2/webkit2.h>

#include "../guile/request.h"
#include "app.h"
#include "buffer.h"
#include "frame.h"
#include "util.h"

static void
web_view_load_changed (WebKitWebView *view, WebKitLoadEvent load_event,
                       gpointer user_data)
{
}

static gboolean
web_view_download_request_cb (WebKitWebView *webView, WebKitDownload *download,
                              gboolean *handled)
{
  return TRUE;
}

static gboolean
decide_policy_cb (WebKitWebView *view, WebKitPolicyDecision *decision,
                  WebKitPolicyDecisionType dtype)
{
  switch (dtype)
    {
    case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
      {
        /* WebKitNavigationAction *nav
         *     = webkit_navigation_policy_decision_get_navigation_action (
         *         WEBKIT_NAVIGATION_POLICY_DECISION (decision)); */
        /* WebKitURIRequest *req = webkit_navigation_action_get_request (nav);
         */
        /* SCM url = scm_from_locale_string (webkit_uri_request_get_uri (req));
         */
        /* scm_nomad_make_buffer (url); */
        return TRUE;
      }
    case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
      {
        WebKitResponsePolicyDecision *policy
            = WEBKIT_RESPONSE_POLICY_DECISION (decision);
        WebKitURIResponse *response
            = webkit_response_policy_decision_get_response (policy);
        if (!webkit_response_policy_decision_is_mime_type_supported (policy))
          {
            const gchar *uri = webkit_uri_response_get_uri (response);
            scm_call_1 (scm_c_public_ref ("nomad download", "download"),
                        scm_from_locale_string (uri));
          }
        return TRUE;
      }
    default:
      return FALSE;
    }

  return TRUE;
}
