#ifndef KEYMAP_H
#define KEYMAP_H

#include <QDebug>
#include <QEvent>
#include <QObject>

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
public slots:
  void handleScrollv (QVariant offset);
  void handleGoBack ();
  void handleGoForward ();
  void handleKeymap (int, int);
  void handleKillBuffer ();
};

#endif // KEYMAP_H
