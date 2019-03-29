#include "app.h"
#include "buffer.h"
#include "frame.h"
#include "keymap.h"
#include "webview.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QQmlProperty>
#include <QQuickStyle>

#include <QVariant>
#include <libguile.h>
#include <qtwebengineglobal.h>

QObject *root = NULL;
QObject *window = NULL;

Keymap keymap;

int
start_app (int argc, char *argv[])
{
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

  // UML signals to C++ methods
  QObject::connect (window, SIGNAL (submitKeymap (int, int)), &keymap,
                    SLOT (handleKeymap (int, int)));

  QObject::connect (window, SIGNAL (submitEval (QString)), &keymap,
                    SLOT (Eval (QString)));

  QObject::connect (window, SIGNAL (handleCompletion (QString)), &keymap,
                    SLOT (Complete (QString)));

  QObject::connect (root, SIGNAL (destroyed ()), &keymap, SLOT (Kill ()));

  // C++ signals to UML methods
  QObject::connect (&keymap, SIGNAL (makeFrame (QVariant)), window,
                    SLOT (makeFrame (QVariant)));

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

  QObject::connect (&keymap, SIGNAL (findText (QString)), currentWebView (),
                    SLOT (findText (QString)));

  return app.exec ();
}

void
inner_main (void *data, int argc, char *argv[])
{
  scm_call_1 (scm_c_public_ref ("nomad util", "add-to-nomad-path"),
              scm_from_utf8_string ("/home/mrosset/src/nomad/scheme"));

  // Define scheme C modules
  // Modules that are used before defining have a scheme file. This
  // allows mixing pure scheme with C scheme.
  scm_c_use_module ("nomad webview");
  scm_c_define_module ("nomad webview", webview_register_functions, NULL);

  // scm_c_define_module ("nomad window", nomad_window_register_functions,
  // NULL);

  scm_c_use_module ("nomad buffer");
  scm_c_define_module ("nomad buffer", buffer_register_functions, NULL);

  scm_c_use_module ("nomad frame");
  scm_c_define_module ("nomad frame", frame_register_functions, NULL);

  // scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad util");
  // scm_c_use_module ("nomad window");
  scm_c_use_module ("nomad browser");
  scm_c_use_module ("nomad repl");

  scm_c_use_module ("nomad init");
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
  // we are the only instance. so start QT a new application as well
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
  setenv ("GUILE_LOAD_COMPILED_PATH", NOMAD_GUILE_LOAD_COMPILED_PATH, 1);
  scm_boot_guile (argc, argv, inner_main, NULL);
}
