/*
 * keymap.h
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
  void updateMap (QVariant bind, QVariant proc);
  void loadHTML (QVariant string);

public slots:
  void UpdateMap (QString map);
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
  void LoadHTML (QVariant string);
};

#endif // KEYMAP_H
