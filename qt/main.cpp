#include "app.h"
#include "buffer.h"
#include "webview.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QVariant>
#include <libguile.h>
#include <qtwebengineglobal.h>

QObject *root = NULL;

static QUrl
startupUrl ()
{
  return QUrl (QStringLiteral ("https://www.gnu.org/software/guile/"));
}

int
start_app (int argc, char *argv[])
{
  QCoreApplication::setOrganizationName ("Nomad");
  QCoreApplication::setAttribute (Qt::AA_EnableHighDpiScaling);

  QApplication app (argc, argv);

  QtWebEngine::initialize ();

  QQmlApplicationEngine engine;

  engine.load (QUrl (QStringLiteral ("qrc:/ApplicationRoot.qml")));
  root = engine.rootObjects ().first ();
  QMetaObject::invokeMethod (root, "load", Q_ARG (QVariant, startupUrl ()));
  return app.exec ();
}

void
inner_main (void *data, int argc, char *argv[])
{
  scm_c_use_module ("nomad init");
  scm_c_eval_string ("(init)");

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
  scm_boot_guile (argc, argv, inner_main, NULL);
}
