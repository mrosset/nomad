import QtQuick 2.11
import QtQuick.Layouts 1.11
import QtQuick.Dialogs 1.1
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.2
import QtQuick.Window 2.1
import QtWebEngine 1.7
import QtWebChannel 1.0
import Keymap 1.0

ApplicationWindow {
    id: browserWindow
    objectName: "browserWindow"
    property QtObject applicationRoot
    property Item currentWebView: tabs.count > 0 ? tabs.getTab(tabs.currentIndex).item: null
    property int previousVisibility: Window.Windowed
    property QtObject currentPopup: null

    signal submitKeymap(string keymap, int modifers, int key)
    signal submitEval(string input);
    signal evalWithArgs(string symbol, string arg0);
    signal handleCompletion(string input);
    signal historyCompletion(string input);
    signal updateMap(string map);

    visible: true
    width: 640
    height: 480

    function handleUpdateMap(bind, proc) {
        currentPopup.handleUpdateMap(bind, proc)
    }
    function showPopUp(keymap, prompt) {
        if (currentPopup != null) {
            currentPopup.destroy()
        }
        var component = Qt.createComponent("MiniPopup.qml")
        if( component.status != Component.Ready )
        {
            if( component.status == Component.Error )
                console.debug("Error:"+ component.errorString() )
            return
        }
        currentPopup = component.createObject(popUpContainer)
        currentPopup.keymap = keymap
        miniBuffer.prompt = prompt
        updateMap(keymap)
    }

    Action {
        shortcut: "Ctrl+h"
        onTriggered: {
            showPopUp("ctrl-h-map", "C-h");
        }
    }
    Action {
        shortcut: "Ctrl+x"
        onTriggered: {
            showPopUp("ctrl-x-map", "C-x");
        }
    }
    Action {
        shortcut: "Alt+m"
        onTriggered: {
            if (webViewLayout.state == "Open" && terminal.focus) {
                return webViewLayout.state = "Close"
            }
            webViewLayout.state = "Open"
        }
    }
    Action {
        shortcut: "Alt+x"
        onTriggered: {
            miniBuffer.focus = !miniBuffer.focus
        }
    }
    Action {
        shortcut: "Ctrl+Shift+d"
        onTriggered: {
            develView.visible = !devToolsView.visible
        }
    }
    Action {
        shortcut: "Ctrl+g"
        onTriggered: {
            keyboardQuit()
        }
    }
    Action {
        shortcut: "Ctrl+f"
        onTriggered: {
            tabs.focus = false
            currentWebView.forceActiveFocus();
            currentWebView.runJavaScript("hintMode();")
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
        spacing: 0
        width: parent.width
        height: parent.height - miniBuffer.height
        id: webViewLayout
        TabView {
            id: tabs
            focus: true
            frameVisible: false
            tabsVisible: true
            Layout.preferredWidth: parent.width
            Layout.fillHeight: true
            style: TabViewStyle {
                tabsMovable: true
                tab: Item {
                    implicitWidth: text.width + 6
                    implicitHeight: text.height + 6
                    /* Image { */
                    /*     id: appIcon */
                    /*     sourceSize: Qt.size(32, 32) */
                    /*     /\* source: webView.icon != "" ? webView.icon : "fallbackFavicon.png"; *\/ */
                    /*     source: currentWebView.icon */
                    /* } */
                    /* color: index === tabs.currentIndex ? "steelblue": "white" */
                    Text {
                        id: text
                        text: index
                        anchors.horizontalCenter: parent.horizontalCenter
                        color: index === tabs.currentIndex ? "steelblue": "black"
                    }
                }

                frame: Rectangle { color: "steelblue" }
            }
            Component.onCompleted: createEmptyTab(defaultProfile)
            Keys.onPressed: {
                submitKeymap("webview-mode-map", event.modifiers, event.key)
            }
            function createEmptyTab(profile) {
                var tab = addTab("", webView);
                tab.active = true;
                /* tab.title = Qt.binding(function() { return currentWebView.focus }); */
                tab.item.profile = profile;
                return tab;
            }
        }
        Rectangle {
            id: develView
            Layout.preferredHeight: parent.height / 4
            Layout.fillWidth: true;
            visible: false
            WebEngineView {
                id: devToolsView
                visible: true
                height: visible ? 400 : 0
                inspectedView: visible && tabs.currentIndex < tabs.count ? tabs.getTab(tabs.currentIndex).item : null
                anchors.fill: parent
                onNewViewRequested: function(request) {
                    var tab = tabs.createEmptyTab(currentWebView.profile);
                    tabs.currentIndex = tabs.count - 1;
                    request.openIn(tab.item);
                }
            }
        }
        RowLayout {
            id: statusRow
            Button {
                id: testButton
                objectName: "testButton"
                text: "debug"
                onClicked: {
                    keymap.someSignal("test")
                }
                visible: false
            }
            Label {
                id: statusUrl
                color: "steelblue"
                text: currentWebView.title
                Layout.fillWidth: true
            }
            ProgressBar {
                id: progress
                Layout.fillWidth: true
                maximumValue: 100
                value: currentWebView.loadProgress
                visible: value < maximumValue ? true: false
            }
            Text {
                color: "steelblue"
                text: "%1%".arg(currentWebView.loadProgress)
            }
        }
    }

    ColumnLayout {
        id: miniBufferLayout
        anchors.bottom: parent.bottom
        width: parent.width
        spacing: 0
        Rectangle {
            id: miniSeperator
            height: 1
            Layout.fillWidth: true
            color: "steelblue"
            visible: miniOutput.visible
        }
        Rectangle {
            id: popUpContainer
            height: 200
            visible: false
            Layout.fillWidth: true
        }
        Rectangle {
            id: miniOutputRect
            color: "white"
            Layout.fillWidth: true
            height: 200
            visible: false
            ListView {
                id: miniOutput
                objectName: "miniOutput"
                anchors.fill: parent
                delegate: Text {
                    width: parent.width
                    text: symbol
                }
                highlight: Rectangle { color: "lightsteelblue"; }
                model: miniBufferModel
                function selectUp() {
                    if (miniOutput.currentIndex == 0 ) {
                        return
                    }
                    miniOutput.currentIndex--
                    miniBuffer.text = miniBufferModel.get(miniOutput.currentIndex).symbol
                }
                function selectDown() {
                    if (miniOutput.currentIndex == miniBufferModel.count - 1) {
                        return
                    }
                    miniOutput.currentIndex++
                    miniBuffer.text = miniBufferModel.get(miniOutput.currentIndex).symbol
                }
            }
            onVisibleChanged: {
                miniOutput.visible = visible
                miniSeperator.visible = visible
            }
        }
        ListModel {
            id: miniBufferModel
            ListElement { symbol: "" }
        }
        Rectangle {
            height: 1
            Layout.fillWidth: true
            color: "steelblue"
            visible: true
        }
        Rectangle {
            id: miniBufferRowRect
            color: "white"
            height: miniBuffer.height
            Layout.fillWidth: true
            RowLayout {
                id: miniBufferRowLayout
                Label {
                    id: miniBufferLabel
                    text: miniBuffer.prompt
                    visible: miniBuffer.focus
                }
                TextInput {
                    id: miniBuffer
                    objectName: "miniBuffer"
                    font.pointSize: 12
                    Layout.fillWidth: true
                    property string prompt: "M-x"
                    property string symbol: null
                    states: [
                        State {
                            name: "prompt"
                            PropertyChanges {
                                target: miniBuffer
                                focus: true
                                text: ""
                                onAccepted: {
                                    if (miniBufferModel.count > 0) {
                                        text = miniBufferModel.get(miniOutput.currentIndex).symbol
                                    }
                                    evalWithArgs(symbol, text)
                                    miniBuffer.state = ""
                                    tabs.focus = true
                                }
                                onTextEdited: {
                                    historyCompletion(miniBuffer.text)
                                }
                            }
                        },
                        State {
                            name: ""
                            PropertyChanges {
                                target: miniBuffer
                                prompt: "M-x"
                                text: ""
                            }
                            PropertyChanges {
                                target: miniBufferLabel
                                text: miniBuffer.prompt
                            }
                            PropertyChanges {
                                target: tabs
                                focus: true
                            }
                        }
                    ]
                    onAccepted: {
                        text = miniBufferModel.get(miniOutput.currentIndex).symbol
                        submitEval(text)
                    }
                    onTextEdited: {
                        handleCompletion(miniBuffer.text)
                    }
                    onFocusChanged: {
                        miniBufferLabel.visible = focus
                        miniBufferModel.clear()
                        if(!focus) {
                            miniOutputRect.visible = false
                            miniBufferTimer.start()
                        }
                        if(focus)
                            handleCompletion("")
                    }
                    Keys.onPressed: {
                        submitKeymap("minibuffer-mode-map", event.modifiers, event.key)
                    }
                }
                Timer {
                    id: miniBufferTimer
                    interval: 5000; running: false; repeat: false
                    onTriggered: {
                        if(!miniBuffer.focus)
                            miniBuffer.text = ""
                    }
                }
            }
        }
    }

    // Components
    Keymap {
        id: keymap
        WebChannel.id: "keymap"
        signal someSignal(string message);
        function kbquit() {
            console.log("quit keyboard")
            keyboardQuit()
        }
    }
    WebChannel {
        id: channel
        registeredObjects: [keymap]
    }
    Component {
        id: webView
        WebView{}
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
        miniBuffer.focus = false
    }

    function clearMiniOutput() {
        miniBufferModel.clear()
    }

    function setMiniOutput(output) {
        miniOutputRect.visible = true
        for (var i in output) {
            miniBufferModel.append({"symbol": output[i]})
        }
    }

    function switchToBuffer(index) {
        tabs.currentIndex = index
    }

    function keyboardQuit() {
        miniBuffer.clear()
        currentWebView.focus = false
        miniBuffer.state = ""
        tabs.focus = true
        popUpContainer.visible = false;
        if(currentPopup != null) {
            currentPopup.destroy();
        }
    }

    function setUrl(uri) {
        currentWebView.url = uri
    }

    function promptInput(cmd, arg) {
        miniBufferModel.clear()
        miniBuffer.prompt = arg + " ?"
        miniBuffer.symbol = cmd
        historyCompletion("")
        miniBuffer.state = "prompt"
    }
}
