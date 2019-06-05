import QtQuick 2.11
import QtQuick.Layouts 1.11
import QMLTermWidget 1.0

QMLTermWidget {
    id: terminal
    visible: true
    Layout.alignment: Qt.AlignBottom
    Layout.preferredWidth: parent.width
    Layout.preferredHeight: parent.height / 4
    font.family: "Monospace"
    font.pointSize: 10
    colorScheme: "cool-retro-term"
    session: QMLTermSession{
	id: mainsession
	property string startSexp: "(progn (geiser-connect-local 'guile \"/tmp/nomad-socket\") (delete-other-windows))"
	initialWorkingDirectory: "/home/mrosset/src/nomad"
	shellProgram: "emacs"
	shellProgramArgs: ["-nw", "-Q", "-l", "/home/mrosset/src/nomad/init.el"]
	/* shellProgram: "nomad" */
	/* shellProgramArgs: ["-c", "--listen", "/tmp/nomad-devel"] */
    }
    MouseArea {
	anchors.fill: parent
	onClicked: {
	    terminal.state = ""
	}
    }
    Component.onCompleted: mainsession.startShellProgram()
    QMLTermScrollbar {
	terminal: terminal
	width: 20
	Rectangle {
	    opacity: 0.4
	    anchors.margins: 5
	    radius: width * 0.5
	    anchors.fill: parent
	}
    }
}
