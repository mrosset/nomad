#ifndef KEYMAP_H
#define KEYMAP_H

#include <QDebug>
#include <QEvent>
#include <QObject>
#include <libguile.h>

class Keymap : public QObject
{
  Q_OBJECT
public:
  explicit Keymap (QObject *parent = nullptr);

signals:
  void scrollv (QVariant offset);
  void goBack ();
  void goForward ();
  void killBuffer ();
  void nextBuffer ();
  void makeFrame (QVariant uri);
  void makeBuffer (QVariant uri);
  void submitEval (QString input);
  void handleComplete (QString input);
  void setMiniOutput (QVariant output);
  void clearMiniOutput ();
  void setMiniBuffer (QVariant output);
  QVariant getBuffer (QVariant index);
  void setUrl (QVariant url);
  void findText (QString text);
  void promptInput (QVariant cmd, QVariant arg);
  void evalWithArgs (QString symbol, QString arg0);

public slots:
  void historyComplete (QString input);
  void Complete (QString input);
  void Eval (QString input);
  void Kill ();
  void MakeFrame (QVariant uri);
  void MakeBuffer (QVariant uri);
  void NextBuffer ();
  void handleScrollv (QVariant offset);
  void handleGoBack ();
  void handleGoForward ();
  void handleKeymap (QString keymap, int modifiers, int key);
  void handleKillBuffer ();
  void handleMessage (QString msg);
  void SetUrl (QVariant uri);
  void EvalWithArgs (QString symbol, QString arg0);
};

#endif // KEYMAP_H
