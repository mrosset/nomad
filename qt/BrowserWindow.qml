import QtQuick 2.2
import QtQuick.Layouts 1.11
import QtQuick.Dialogs 1.1
import QtQuick.Controls 1.2
import QtQuick.Window 2.1
import QtWebEngine 1.0
import QMLTermWidget 1.0

ApplicationWindow {
    id: browserWindow
    property QtObject applicationRoot
    property WebEngineView currentWebView: tabs.currentIndex < tabs.count ? tabs.getTab(tabs.currentIndex).item : null
    property int previousVisibility: Window.Windowed

    visible: true

    width: 640
    height: 480
    ColumnLayout {
        spacing: 1
        anchors.fill: parent
        TabView {
            id: tabs
            Layout.alignment: Qt.AlignTop
            Layout.preferredWidth: parent.width
            Layout.preferredHeight: parent.height / 2
            function createEmptyTab(profile) {
                var tab = addTab("", webView);
                // We must do this first to make sure that tab.active gets set so that tab.item gets instantiated immediately.
                tab.active = true;
                tab.title = Qt.binding(function() { return tab.item.title });
                tab.item.profile = profile;
                return tab;
            }
            Component.onCompleted: createEmptyTab(defaultProfile)
        }
        Rectangle {
            Layout.alignment: Qt.AlignBottom
            Layout.preferredWidth: parent.width
            Layout.preferredHeight: parent.height / 4
            QMLTermWidget {
                id: terminal
                anchors.fill: parent
                font.family: "Monospace"
                font.pointSize: 8
                colorScheme: "SolarizedLight"
                session: QMLTermSession{
                    id: mainsession
                    initialWorkingDirectory: "$HOME"
                    onMatchFound: {
                        console.log("found at: %1 %2 %3 %4".arg(startColumn).arg(startLine).arg(endColumn).arg(endLine));
                    }
                    onNoMatchFound: {
                        console.log("not found");
                    }
                }
                Component.onCompleted: mainsession.startShellProgram();
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
            Component.onCompleted: terminal.forceActiveFocus();
        }
    }

    function currentUrl() {
        return this.currentWebView.url;
    }

    Component {
        id: webView
        WebEngineView {
            focus: true
        }
    }
 }
