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
            terminal.visible = !terminal.visible;
            if(terminal.visible) {
                tabs.focus = false;
                terminal.focus = true;
                currentWebView.focus = false;
            } else {
                tabs.focus = true;
                terminal.focus = false;
                currentWebView.focus = false;
            }
            tabs.updateSize();
        }
    }
    ColumnLayout {
        spacing: 1
        width: parent.width
        height: parent.height
        id: column
        TabView {
            id: tabs
            Layout.alignment: Qt.AlignTop
            Layout.preferredWidth: parent.width
            Layout.preferredHeight: parent.height - terminal.height
            function createEmptyTab(profile) {
                var tab = addTab("", webView);
                // We must do this first to make sure that tab.active gets set so that tab.item gets instantiated immediately.
                tab.active = true;
                tab.title = Qt.binding(function() { return tabs.focus });
                statusTitle.text = Qt.binding(function() { return tab.item.title });
                statusUrl.text  = Qt.binding(function() { return tab.item.url });
                tab.item.profile = profile;
                return tab;
            }
            Component.onCompleted: createEmptyTab(defaultProfile)
            function updateSize() {
                if (!terminal.visible) {
                    this.Layout.preferredHeight = parent.height - statusRow.height;
                } else {
                    this.Layout.preferredHeight = parent.height - terminal.height - statusRow.height;
                }
            }
            Keys.onPressed: {
                submitKeymap(event.modifiers, event.key)
                /* ; event.modifers, event.key) */

                /* if (event.modifiers == Qt.ControlModifier) { */
                /*     switch(event.key) { */
                /*     case Qt.Key_N: */
                /*         scrollv(100) */
                /*         break */
                /*     case Qt.Key_U: */
                /*          goBack() */
                /*         break */
                /*     case Qt.Key_M: */
                /*         goForward() */
                /*         break */
                /*     case Qt.Key_P: */
                /*         scrollv(-100) */
                /*         break */
                /*     case Qt.Key_R: */
                /*         currentWebView.reload() */
                /*         break */
                /*     case Qt.Key_B: */
                /*         nextBuffer() */
                /*         break */
                /*     case Qt.Key_X: */
                /*         killBuffer() */
                /*         break */
                /*     } */
                /* } */
            }
        }
        RowLayout {
            id: statusRow
            Label {
                color: "steelblue"
                id: statusTitle
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width / 2
            }
            /* Button { */
            /*     id: testButton */
            /*     text: "press" */
            /*     onClicked: killBuffer() */
            /* } */
            Label {
                id: statusUrl
                color: "steelblue"
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
            colorScheme: "BreezeModified"
            session: QMLTermSession{
                id: mainsession
                initialWorkingDirectory: "/home/mrosset/src/nomad"
                onMatchFound: {
                    console.log("found at: %1 %2 %3 %4".arg(startColumn).arg(startLine).arg(endColumn).arg(endLine));
                }
                onNoMatchFound: {
                    console.log("not found");
                }
            }
            Component.onCompleted: start();
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
            function start() {
                mainsession.startShellProgram();
            }
        }
        Component.onCompleted: terminal.forceActiveFocus();
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

    Component {
        id: webView
        WebEngineView {
            function setUrl(uri) {
                url =  uri
            }
        }
    }
 }
