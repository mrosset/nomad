---
title: "Hack Nomad"
date: 2019-05-17
draft: false
author: amar-m-singh
template: article.pug
---

Nomad is hackable in Scheme. But you can also hack Nomad in Cpp/QML.
In general, the aim of Nomad is to present you the user with enough
programming capabilities that you don\'t have to resort to this step,
but you may still wish to change some internal plumbing in Nomad or
expose new functionality to Scheme, and we encourage you in this
endeavour.

## Hack Nomad in QML

-   One design principles we can use as a of rule of thumb is, for every
    feature provided by Nomad in the standard library or in the
    application, user should be able to provide their replacement.
-   You can find features available in WebEngineView at [QtWebEngineView
    Features](https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html#Feature-prop).

## To implement a Method from Qtwebengine

-   You can add a procedure in Nomad Cpp modules that will become
    accessible from Scheme with \`SCM~DEFINE~\` from the Guile\'s C
    library \`libguile\`.

        #include <libguile.h>

-   The \`currentWebView\` is an instance of Qt WebEngineView, it can
    support a function called \`loadHtml\`, which loads HTML into the
    WebView. But, we can also use it to load any type of data!
-   To make this facility available to Scheme, we\'d like to define a
    procedure called \`webview-load-string\`, that can wrap up this
    functionality and make it available at Nomad\'s runtime.
-   The following \`webview-load-string\` procedure is one of the ways
    you can do it. Webview-load-string is converting it\'s argument, a
    Scheme object into a Qt object which can be understood by
    Qtwebengine.

        SCM_DEFINE (scm_webview_load_string, "webview-load-string", 1, 0, 0, (SCM html),
                  "Set's the current WebView to string HTML.")
        {
          QMetaObject::invokeMethod(currentWebView(), "loadHtml",
            Qt::DirectConnection, Q_ARG(QString, (scm_to_qstring (html))));

          return SCM_UNSPECIFIED;
        }

-   Then, we invoke the loadHtml Method on this argument.. Although,
    this approach may work, the results can be less than robust.
-   It\'s good practice to give your function an unabbreivated and
    descriptive name and a documentation string explaining how to use
    the procedure.
-   To export this procedure use:

        scm_c_export ("webview-load-string");

-   Then you can compile Nomad and add this to your \`\$HOME/.nomad\`

    ``` {.scheme}
    (define-command (say-hello) (webview-load-string "Hello World"))
    ```

-   And in Nomad, M-x say-hello

## Implement loadHTML as a signal

-   The following series of
    [diffs](https://www.gnu.org/software/diffutils/manual/diffutils.html#Unified-Format),
    should elaborate the task at hand. You may think this is a lot of
    boilerplate for a simple change, using keymap.cpp makes the task
    somewhat easier.
-   First, we will define a loadHTML function in BrowserWindow. loadHTML
    simply calls loadHtml. Currently browser url \"qrc:/\" is
    hard-coded, unless someone can find a use for something else as an
    argument. The capitalisations are important.
-   modified qt/BrowserWindow.qml

``` {.diff}
@@ -565,4 +565,7 @@ ApplicationWindow {
         historyCompletion("")
         miniBuffer.state = "prompt"
     }
+        function loadHTML(string) {
+        currentWebView.loadHtml(string , "qrc:/")
+    }
 }
```

-   Keymap will help us generate a \`signal\` from a procedure call.
-   modified qt/keymap.cpp

``` {.diff}
@@ -218,3 +218,9 @@ Keymap::UpdateMap (QString map)
       emit updateMap (bind, proc);
     }
 }
+
+void
+Keymap::LoadHTML (QVariant string)
+{
+  emit loadHTML (string);
+}
```

-   modified qt/keymap.h

``` {.diff}
@@ -51,6 +51,7 @@ signals:
   void promptInput (QVariant cmd, QVariant arg);
   void evalWithArgs (QString symbol, QString arg0);
   void updateMap (QVariant bind, QVariant proc);
+  void loadHTML (QVariant string);

 public slots:
   void UpdateMap (QString map);
@@ -69,6 +70,7 @@ public slots:
   void handleMessage (QString msg);
   void SetUrl (QVariant uri);
   void EvalWithArgs (QString symbol, QString arg0);
+  void LoadHTML (QVariant string);
 };

 #endif // KEYMAP_H
```

-   modified qt/main.cpp

``` {.diff}
@@ -97,6 +97,9 @@ window_signals (QObject *window)

   QObject::connect (&keymap, SIGNAL (promptInput (QVariant, QVariant)), window,
                     SLOT (promptInput (QVariant, QVariant)));
+
+  QObject::connect (&keymap, SIGNAL (loadHTML (QVariant)), window,
+                    SLOT (loadHTML (QVariant)));
 }

 static void
```

-   Then \`webview-load-string\` in qt/webview.cpp will change to

<!-- -->

    SCM_DEFINE (scm_webview_load_string, "webview-load-string", 1, 0, 0, (SCM string),
                "Set's the current WebView to display STRING.")
    {
      QVariant arg = QVariant (scm_to_qstring (string));

      keymap.LoadHTML (arg);
      return SCM_UNSPECIFIED;
    }

-   To use (webview-load-string) in Nomad.
-   The result <http://piviq.ga:9001/images/nomad-display.png>
