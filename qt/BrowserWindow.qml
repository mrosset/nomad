import QtQuick 2.2
import QtQuick.Layouts 1.11
import QtQuick.Dialogs 1.1
import QtQuick.Controls 1.2
import QtQuick.Window 2.1
import QtWebEngine 1.7
import QMLTermWidget 1.0

ApplicationWindow {
    id: browserWindow
    objectName: "browserWindow"
    property QtObject applicationRoot
    property Item currentWebView: tabs.count > 0 ? tabs.getTab(tabs.currentIndex).item: null
    property int previousVisibility: Window.Windowed

    signal submitKeymap(int modifers, int key)

    visible: true

    width: 640
    height: 480

    Action {
        shortcut: "Alt+m"
        onTriggered: {
            if (layout.state == "" && terminal.focus) {
                return layout.state = "Close"
            }
            currentWebView.focus = false
            terminal.forceActiveFocus()
            layout.state = ""
        }
    }

    Action {
        shortcut: "Ctrl+i"
        onTriggered: {
            tabs.focus = true
            currentWebView.focus = false
        }
    }

    Action {
        shortcut: "Escape"
        onTriggered: {
            if (currentWebView.state == "FullScreen") {
                browserWindow.visibility = browserWindow.previousVisibility;
                fullScreenNotification.hide();
                currentWebView.triggerWebAction(WebEngineView.ExitFullScreen);
            }
        }
    }

    FullScreenNotification {
        id: fullScreenNotification
    }

    ColumnLayout {
        spacing: 1
        width: parent.width
        height: parent.height
        id: layout
        TabView {
            id: tabs
            Layout.alignment: Qt.AlignTop
            Layout.preferredWidth: parent.width
            Layout.preferredHeight: parent.height - terminal.height - statusRow.height
            function createEmptyTab(profile) {
                var tab = addTab("", webView);
                tab.active = true;
                tab.title = Qt.binding(function() { return currentWebView.focus });
                statusUrl.text  = Qt.binding(function() { return tab.item.url });
                tab.item.profile = profile;
                return tab;
            }
            Component.onCompleted: createEmptyTab(defaultProfile)
            Keys.onPressed: {
                submitKeymap(event.modifiers, event.key)
            }
        }
        RowLayout {
            id: statusRow
            Button {
                id: testButton
                text: "debug"
                onClicked: {
                    killBuffer()
                }
                visible: false
            }
            Label {
                id: statusUrl
                color: "steelblue"
                Layout.fillWidth: true
            }
            Text {
                color: "steelblue"
                text: "tabs: %1 terminal: %2 browser: %3".arg(tabs.focus).arg(terminal.focus).arg(currentWebView.focus)
                Layout.alignment: Qt.AlignRight
            }
        }
        QMLTermWidget {
            id: terminal
            focus: true
            Layout.alignment: Qt.AlignBottom
            Layout.preferredWidth: parent.width
            Layout.preferredHeight: parent.height / 4
            font.family: "Monospace"
            font.pointSize: 10
            colorScheme: "DarkPastels"
            session: QMLTermSession{
                id: mainsession
                initialWorkingDirectory: "/home/mrosset/src/nomad"
                shellProgram: "emacsclient"
                shellProgramArgs: ["-nw", "-e", "(progn (geiser-connect-local 'guile \"/tmp/nomad-socket\")(delete-other-windows))"]
            }
            MouseArea {
                anchors.fill: parent
                onClicked: {
                    console.log("click")
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
        states: [
            State {
                name: ""
                PropertyChanges {
                    target: terminal
                    visible: true
                    focus: true
                }
                PropertyChanges {
                    target: tabs
                    Layout.preferredHeight: browserWindow.height - terminal.height - statusRow.height
                    focus: false
                }
                PropertyChanges {
                    target: currentWebView
                    focus: false
                }
            },
            State {
                name: "Close"
                PropertyChanges {
                    target: terminal
                    visible: false
                    focus: false
                }
                PropertyChanges {
                    target: tabs
                    Layout.preferredHeight: browserWindow.height - statusRow.height
                    focus: true
                }
                PropertyChanges {
                    target: currentWebView
                    focus: false
                }
            }
        ]
    }

    function scrollv(y) {
        var method = "window.scrollBy(0, %1)".arg(y)
        currentWebView.runJavaScript(method)

    }

    function makeBuffer(url) {
        tabs.createEmptyTab(defaultProfile);
        tabs.currentIndex++
        currentWebView.url = url;
    }

    function killBuffer() {
        if(tabs.count != 1) {
            tabs.removeTab(tabs.currentIndex)
        }
    }

    function nextBuffer() {
        tabs.currentIndex = tabs.currentIndex < tabs.count - 1 ? tabs.currentIndex + 1: 0
    }

    function goBack() {
        currentWebView.goBack();
    }

    function goForward() {
        currentWebView.goForward();
    }

    function totalBuffers( ) {
        return tabs.count
    }

    function getBuffer(index) {
        return tabs.getTab(index).item.url
     }

    Component {
        id: webView
        WebEngineView {
            id: webEngineView
            states: [
                State {
                    name: "FullScreen"
                    PropertyChanges {
                        target: tabs
                        frameVisible: false
                        tabsVisible: false
                        Layout.preferredHeight: parent.height
                    }
                    PropertyChanges {
                        target: statusRow
                        visible: false
                    }
                    PropertyChanges {
                        target: terminal
                        visible: false
                    }
                }
            ]
            function setUrl(uri) {
                url =  uri
            }
            onFullScreenRequested: function(request) {
                if (request.toggleOn) {
                    webEngineView.state = "FullScreen";
                    browserWindow.previousVisibility = browserWindow.visibility;
                    browserWindow.showFullScreen();
                    fullScreenNotification.show();
                } else {
                    webEngineView.state = "";
                    browserWindow.visibility = browserWindow.previousVisibility;
                    fullScreenNotification.hide();
                }
                request.accept();
            }
        }
    }
 }
