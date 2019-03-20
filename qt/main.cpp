#include "app.h"
#include "buffer.h"
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

static QUrl
startupUrl ()
{
  return QUrl (QStringLiteral ("http://localhost:6060"));
}

int
start_app (int argc, char *argv[])
{
  QCoreApplication::setOrganizationName ("Nomad");
  QCoreApplication::setAttribute (Qt::AA_EnableHighDpiScaling);

  QApplication app (argc, argv);
  QQuickStyle::setStyle ("Material");

  QtWebEngine::initialize ();

  QQmlApplicationEngine engine;

  engine.load (QUrl (QStringLiteral ("qrc:/ApplicationRoot.qml")));
  root = engine.rootObjects ().first ();

  // set nomad directory
  SCM nomad = scm_fluid_ref(scm_c_public_ref ("nomad init", "user-nomad-directory"));
  QVariant arg = QVariant (scm_to_locale_string(nomad));
  QMetaObject::invokeMethod (root, "setNomadDir",
                             Q_ARG (QVariant, arg));

  QMetaObject::invokeMethod (root, "load", Q_ARG (QVariant, startupUrl ()));

  window = qvariant_cast<QObject *> (QQmlProperty::read (root, "window"));

  // UML signals to C++ methods
  QObject::connect (window, SIGNAL (submitKeymap (int, int)), &keymap,
                    SLOT (handleKeymap (int, int)));

  // C++ signals to UML methods
  QObject::connect (&keymap, SIGNAL (nextBuffer ()), window,
                    SLOT (nextBuffer ()));

  QObject::connect (&keymap, SIGNAL (scrollv (QVariant)), window,
                    SLOT (scrollv (QVariant)));

  QObject::connect (&keymap, SIGNAL (goBack ()), window, SLOT (goBack ()));

  QObject::connect (&keymap, SIGNAL (goForward ()), window,
                    SLOT (goForward ()));

  QObject::connect (&keymap, SIGNAL (killBuffer ()), window,
                    SLOT (killBuffer ()));

  // QMetaObject::invokeMethod (window, "submitKeymap", Q_ARG (QString,
  // "RAWR"));

  // QObject::connect (&keymap, SIGNAL (setTextField (QVariant)), window,
  //                  SLOT (setTextField (QVariant)));

  // print_methods (currentWebView);
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

  // scm_c_define_module ("nomad window", nomad_window_register_functions,
  // NULL);

  scm_c_use_module ("nomad buffer");
  scm_c_define_module ("nomad buffer", buffer_register_functions, NULL);

  // scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad util");
  // scm_c_use_module ("nomad window");
  scm_c_use_module ("nomad browser");
  scm_c_use_module ("nomad repl");

  scm_c_use_module ("nomad init");
  scm_c_eval_string ("(init)");

  // FIXME: users can start REPL via user-init-hook in $HOME/.nomad. Add
  // documentation for $HOME/.nomad
  scm_c_run_hook (scm_c_public_ref ("nomad init", "user-init-hook"), NULL);
  scm_c_eval_string ("(server-start-coop)");

  // scm_putenv (scm_from_locale_string ("SHELL=/usr/bin/emacs"));
  exit (start_app (argc, argv));
}

int
main (int argc, char *argv[])
{
  setenv ("GUILE_LOAD_COMPILED_PATH", NOMAD_GUILE_LOAD_COMPILED_PATH, 1);
  scm_boot_guile (argc, argv, inner_main, NULL);
}
