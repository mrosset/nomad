import QtQuick 2.11
import QtQuick.Layouts 1.11
import QtWebEngine 1.7

WebEngineView {
    id: webEngineView
    webChannel: channel
    userScripts: [
	WebEngineScript{
	    injectionPoint: WebEngineScript.DocumentReady
	    sourceUrl: "qrc:///qtwebchannel/qwebchannel.js"
	    worldId: WebEngineScript.MainWorld
	},
	WebEngineScript{
	    injectionPoint: WebEngineScript.DocumentReady
	    sourceUrl: "qrc:///backend.js"
	    worldId: WebEngineScript.MainWorld
	},
	WebEngineScript{
	    injectionPoint: WebEngineScript.Deferred
	    sourceUrl: "qrc:///hints.js"
	    worldId: WebEngineScript.MainWorld
	}
    ]
    onFocusChanged: {
	if(focus) {
	    focus: false
	    tabs.focus = true
	}
    }
    states: [
	State {
	    name: "FullScreen"
	    PropertyChanges {
		target: miniBufferLayout
		visible: false
	    }
	    PropertyChanges {
		target: webViewLayout
		height: window.height
	    }
	    PropertyChanges {
		target: tabs
		frameVisible: false
		tabsVisible: false
		Layout.preferredHeight: parent.height
		height: window.height
	    }
	    PropertyChanges {
		target: statusRow
		visible: false
	    }
	    PropertyChanges {
		target: terminal
		visible: false
	    }
	    PropertyChanges {
		target: miniBuffer
		visible: false
	    }
	    PropertyChanges {
		target: miniOutput
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
