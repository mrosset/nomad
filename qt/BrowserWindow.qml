import QtQuick 2.11
import QtQuick.Layouts 1.11
import QtQuick.Dialogs 1.1
import QtQuick.Controls 1.2
import QtQuick.Window 2.1
import QtWebEngine 1.7
import QMLTermWidget 1.0
import Keymap 1.0

ApplicationWindow {
    id: browserWindow
    objectName: "browserWindow"
    property QtObject applicationRoot
    property Item currentWebView: tabs.count > 0 ? tabs.getTab(tabs.currentIndex).item: null
    property int previousVisibility: Window.Windowed

    signal submitKeymap(int modifers, int key)
    signal submitEval(string input);
    signal handleCompletion(string input);

    visible: true

    width: 640
    height: 480

    Action {
        shortcut: "Alt+m"
        onTriggered: {
            if (layout.state == "Open" && terminal.focus) {
                return layout.state = "Close"
            }
            currentWebView.focus = false
            terminal.forceActiveFocus()
            layout.state = "Open"
        }
    }

    Action {
        shortcut: "Alt+x"
        onTriggered: {
            miniBuffer.focus = !miniBuffer.focus
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
            focus: true
            frameVisible: false
            /* tabsVisible: false */
            Layout.alignment: Qt.AlignTop
            Layout.preferredWidth: parent.width
            Layout.fillHeight: true
            function createEmptyTab(profile) {
                var tab = addTab("", webView);
                tab.active = true;
                tab.title = Qt.binding(function() { return currentWebView.focus });
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
                text: currentWebView.url
            }
            Text {
                color: "steelblue"
                text: "mini: %4 tabs: %1 terminal: %2 browser: %3".arg(tabs.focus).arg(terminal.focus).arg(currentWebView.focus).arg(miniBuffer.focus)
                Layout.alignment: Qt.AlignRight
            }
        }
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
        Component.onCompleted: { console.log("state", state)}
        state: "Close"
        states: [
            State {
                name: "Open"
                PropertyChanges {
                    target: terminal
                    visible: true
                    focus: true
                }
                PropertyChanges {
                    target: tabs
                    focus: false
                }
                PropertyChanges {
                    target: currentWebView
                    focus: false
                }
                PropertyChanges {
                    target: miniBuffer
                    visible: false
                }
                PropertyChanges {
                    target: miniOutput
                    visible: false
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
                    focus: true
                }
                PropertyChanges {
                    target: miniBuffer
                    visible: true
                }
                PropertyChanges {
                    target: miniOutput
                    visible: false
                }
            }
        ]
        Keymap {
            id: keymap
        }
        RowLayout {
            id: miniBufferLayout
            Label {
                id: miniBufferLabel
                text: "M-x"
                visible: miniBuffer.focus
            }

            TextInput {
                id: miniBuffer
                font.pointSize: 12
                width: parent.width
                color: "steelblue"
                /* Layout.preferredWidth: parent.width */
                onAccepted: {
                    console.log(miniOutput.currentIndex)
                    if (miniOutput.currentIndex >= 0)  {
                        text = miniBufferModel.get(miniOutput.currentIndex).symbol
                    }
                    submitEval(text)
                    setMiniOutput("")
                    tabs.focus = true
                }
                onTextEdited: {
                    handleCompletion(miniBuffer.text)
                }
                onFocusChanged: {
                    miniBufferModel.clear()
                    miniOutput.visible = false
                    if(!miniBuffer.focus) {
                        miniBufferTimer.start()
                    }
                }
                Action {
                    shortcut: "Ctrl+g"
                    onTriggered: {
                        tabs.focus = true
                        miniBuffer.text = ""
                        miniBufferTimer.stop()
                    }
                }
                Action {
                    shortcut: "Alt+n"
                    onTriggered: {
                        miniOutput.currentIndex++
                        miniBuffer.text = miniBufferModel.get(miniOutput.currentIndex).symbol
                    }
                }
                Action {
                    shortcut: "Alt+p"
                    onTriggered: {
                        miniOutput.currentIndex--
                        miniBuffer.text = miniBufferModel.get(miniOutput.currentIndex).symbol
                    }
                }
            }
            Timer {
                id: miniBufferTimer
                interval: 5000; running: false; repeat: false
                onTriggered: miniBuffer.text = ""
            }
        }
        ListView {
            id: miniOutput
            visible: false
            height: 150
            width: parent.width
            delegate: Text {
                width: parent.width
                text: symbol
            }
            highlight: Rectangle { color: "lightsteelblue"; }
            model: miniBufferModel
        }
        ListModel {
            id: miniBufferModel
            ListElement { symbol: "" }
        }
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
            onNewViewRequested: function(request) {
                if (!request.userInitiated)
                    print("Warning: Blocked a popup window.");
                else if (request.destination === WebEngineView.NewViewInTab) {
                    var tab = tabs.createEmptyTab(currentWebView.profile);
                    tabs.currentIndex = tabs.count - 1;
                    request.openIn(tab.item);
                } else if (request.destination === WebEngineView.NewViewInBackgroundTab) {
                    var backgroundTab = tabs.createEmptyTab(currentWebView.profile);
                    request.openIn(backgroundTab.item);
                } else if (request.destination === WebEngineView.NewViewInDialog) {
                    var dialog = applicationRoot.createDialog(currentWebView.profile);
                    request.openIn(dialog.currentWebView);
                } else {
                    var window = applicationRoot.createWindow(currentWebView.profile);
                    request.openIn(window.currentWebView);
                }
            }
        }
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

    function setMiniBuffer(output) {
        miniBuffer.text = output
    }

    function clearMiniOutput() {
        miniOutput.visible = false
        miniBufferModel.clear()
    }

    function setMiniOutput(output) {
        miniOutput.visible = true
        for (var i in output) {
            miniBufferModel.append({"symbol": output[i]})
        }
    }

    function switchToBuffer(index) {
        tabs.currentIndex = index
    }

    function setUrl(uri) {
        currentWebView.url = uri
    }
 }
