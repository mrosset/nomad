/*
 * main.cpp
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
#include "app.h"
#include "buffer.h"
#include "frame.h"
#include "keymap.h"
#include "minibuffer.h"
#include "qml.h"
#include "webview.h"

#include <QApplication>
#include <QLibraryInfo>
#include <QQmlApplicationEngine>
#include <QQmlProperty>
#include <QQuickStyle>
#include <QVariant>
#include <libguile.h>
#include <qtwebengineglobal.h>

QObject *root = NULL;
QObject *window = NULL;

Keymap keymap;

static void
window_signals (QObject *window)
{
  // UML signals to C++ methods
  QObject::connect (window, SIGNAL (submitKeymap (QString, int, int)), &keymap,
                    SLOT (handleKeymap (QString, int, int)));

  QObject::connect (window, SIGNAL (submitEval (QString)), &keymap,
                    SLOT (Eval (QString)));

  QObject::connect (window, SIGNAL (handleCompletion (QString)), &keymap,
                    SLOT (Complete (QString)));

  QObject::connect (window, SIGNAL (historyCompletion (QString)), &keymap,
                    SLOT (historyComplete (QString)));

  QObject::connect (window, SIGNAL (evalWithArgs (QString, QString)), &keymap,
                    SLOT (EvalWithArgs (QString, QString)));

  QObject::connect (window, SIGNAL (updateMap (QString)), &keymap,
                    SLOT (UpdateMap (QString)));

  // C++ signals to UML methods
  QObject::connect (&keymap, SIGNAL (makeBuffer (QVariant)), window,
                    SLOT (makeBuffer (QVariant)));

  QObject::connect (&keymap, SIGNAL (getBuffer (QVariant)), window,
                    SLOT (getBuffer (QVariant)));

  QObject::connect (&keymap, SIGNAL (nextBuffer ()), window,
                    SLOT (nextBuffer ()));

  QObject::connect (&keymap, SIGNAL (scrollv (QVariant)), window,
                    SLOT (scrollv (QVariant)));

  QObject::connect (&keymap, SIGNAL (goBack ()), window, SLOT (goBack ()));

  QObject::connect (&keymap, SIGNAL (goForward ()), window,
                    SLOT (goForward ()));

  QObject::connect (&keymap, SIGNAL (killBuffer ()), window,
                    SLOT (killBuffer ()));

  QObject::connect (&keymap, SIGNAL (setMiniBuffer (QVariant)), window,
                    SLOT (setMiniBuffer (QVariant)));

  QObject::connect (&keymap, SIGNAL (setMiniOutput (QVariant)), window,
                    SLOT (setMiniOutput (QVariant)));

  QObject::connect (&keymap, SIGNAL (clearMiniOutput ()), window,
                    SLOT (clearMiniOutput ()));

  QObject::connect (&keymap, SIGNAL (setUrl (QVariant)), window,
                    SLOT (setUrl (QVariant)));

  QObject::connect (&keymap, SIGNAL (promptInput (QVariant, QVariant)), window,
                    SLOT (promptInput (QVariant, QVariant)));
}

static void
root_signals (QObject *root)
{
  // UML signals to C++ methods
  QObject::connect (root, SIGNAL (destroyed ()), &keymap, SLOT (Kill ()));
}

static void
aux_signals ()
{
  QObject *miniPopup = window->findChild<QObject *> ("miniPopup");
  QObject::connect (&keymap, SIGNAL (updateMap (QVariant, QVariant)),
                    miniPopup, SLOT (handleUpdateMap (QVariant, QVariant)));
}

int
start_app (int argc, char *argv[])
{
  qDebug () << "DataPath" << QLibraryInfo::location (QLibraryInfo::DataPath);
  qDebug () << "TranslationsPath"
            << QLibraryInfo::location (QLibraryInfo::DataPath);

  QCoreApplication::setOrganizationName ("Nomad");
  QCoreApplication::setAttribute (Qt::AA_EnableHighDpiScaling);

  qmlRegisterType<Keymap> ("Keymap", 1, 0, "Keymap");

  QApplication app (argc, argv);
  QQuickStyle::setStyle ("Material");

  QtWebEngine::initialize ();

  QQmlApplicationEngine engine;
  engine.load (QUrl (QStringLiteral ("qrc:/ApplicationRoot.qml")));
  root = engine.rootObjects ().first ();

  // set nomad directory
  SCM nomad = scm_fluid_ref (
      scm_c_public_ref ("nomad init", "user-nomad-directory"));
  QVariant arg = QVariant (scm_to_locale_string (nomad));
  QMetaObject::invokeMethod (root, "setNomadDir", Q_ARG (QVariant, arg));

  scm_nomad_make_frame (
      scm_c_public_ref ("nomad browser", "default-home-page"));

  window = qvariant_cast<QObject *> (QQmlProperty::read (root, "window"));

  window_signals (window);
  root_signals (root);
  aux_signals ();
  // C++ signals to UML methods
  QObject::connect (&keymap, SIGNAL (findText (QString)), currentWebView (),
                    SLOT (findText (QString)));

  return app.exec ();
}

void
inner_main (void *data, int argc, char *argv[])
{
  // Define scheme C modules
  // Modules that are used before defining have a scheme file. This
  // allows mixing pure scheme with C scheme.
  scm_c_use_module ("nomad webview");
  scm_c_define_module ("nomad webview", webview_register_functions, NULL);

  scm_c_use_module ("nomad minibuffer");
  scm_c_define_module ("nomad minibuffer", minibuffer_register_functions,
                       NULL);

  // scm_c_define_module ("nomad window", nomad_window_register_functions,
  // NULL);

  scm_c_use_module ("nomad buffer");
  scm_c_define_module ("nomad buffer", buffer_register_functions, NULL);

  scm_c_use_module ("nomad frame");
  scm_c_define_module ("nomad frame", frame_register_functions, NULL);

  scm_c_define_module ("nomad qml", qml_register_functions, NULL);

  // scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad browser");
  scm_c_use_module ("nomad eval");
  scm_c_use_module ("nomad init");
  scm_c_use_module ("nomad qml");
  scm_c_use_module ("nomad repl");
  scm_c_use_module ("nomad util");
  scm_c_eval_string ("(init)");

  scm_c_use_module ("nomad options");

  SCM socket = scm_c_eval_string ("(option-listen (command-line))");
  SCM url = scm_c_eval_string ("(option-url (command-line))");

  setenv ("NOMAD_SOCKET_FILE", scm_to_utf8_string (socket), 1);

  // FIXME: run his after GUI has been started
  scm_c_run_hook (scm_c_public_ref ("nomad init", "user-init-hook"), NULL);

  SCM exists
      = scm_call_1 (scm_c_public_ref ("nomad repl", "socket-exists?"), socket);

  // start a socket server if one does not exist already. this implies
  // we are the only instance. then start QT application
  if (exists == SCM_BOOL_F)
    {
      // FIXME: users can start REPL via user-init-hook in $HOME/.nomad. Add
      // documentation for $HOME/.nomad
      scm_call_1 (scm_c_public_ref ("nomad repl", "server-start-coop"),
                  socket);
      exit (start_app (0, NULL));
    }

  // when requesting a client start a terminal REPL
  if (scm_c_eval_string ("(option-client (command-line))") == SCM_BOOL_T)
    {
      scm_call_1 (scm_c_public_ref ("nomad repl", "client-start"), socket);
      exit (0);
    }

  // // reuse existing socket then exit
  scm_call_2 (scm_c_public_ref ("nomad buffer", "make-buffer-socket"), url,
              socket);
  sleep (1);
  exit (0);
}

int
main (int argc, char *argv[])
{
  // FIXME: is there a better way to do this?
  setenv ("GUILE_LOAD_COMPILED_PATH", NOMAD_GUILE_LOAD_COMPILED_PATH, 1);
  scm_boot_guile (argc, argv, inner_main, NULL);
}
