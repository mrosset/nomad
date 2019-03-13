#include <QApplication>
#include <QQmlApplicationEngine>
#include <libguile.h>
#include <qtwebengineglobal.h>
#include <stdlib.h>

void
init_guile ()
{
  scm_c_use_module ("nomad init");
  scm_c_use_module ("nomad init");
  scm_c_eval_string ("(init)");

  // Define scheme C modules
  // Modules that are used before defining have a scheme file. This
  // allows mixing pure scheme with C scheme.
  scm_c_use_module ("nomad webkit");
  // scm_c_define_module ("nomad webkit", nomad_webkit_register_functions,
  // NULL);

  // scm_c_define_module ("nomad window", nomad_window_register_functions,
  // NULL);

  scm_c_use_module ("nomad buffer");
  // scm_c_define_module ("nomad buffer", nomad_buffer_register_functions,
  // NULL);

  // scm_c_define_module ("nomad util", nomad_util_register_functions, NULL);

  // Use essential modules
  scm_c_use_module ("nomad util");
  // scm_c_use_module ("nomad window");
  // scm_c_use_module ("nomad browser");
  scm_c_use_module ("nomad repl");

  // FIXME: users can start REPL via user-init-hook in $HOME/.nomad. Add
  // documentation for $HOME/.nomad
  scm_c_run_hook (scm_c_public_ref ("nomad init", "user-init-hook"), NULL);
  scm_c_eval_string ("(server-start-coop)");
}

void
inner_main (void *data, int argc, char **argv)

{
  init_guile ();
  setenv ("GUILE_LOAD_COMPILED_PATH", NOMAD_GUILE_LOAD_COMPILED_PATH, true);
  QCoreApplication::setOrganizationName ("Nomad");
  QCoreApplication::setAttribute (Qt::AA_EnableHighDpiScaling);
  QGuiApplication app (argc, argv);

  QtWebEngine::initialize ();

  QQmlApplicationEngine engine;
  engine.load (QUrl (QStringLiteral ("qrc:/main.qml")));
  exit(app.exec());
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);
  // return app.exec();
}
