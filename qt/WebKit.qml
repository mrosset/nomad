import QtQuick 2.0
import QtQuick.Layouts 1.11
import QtWebKit 3.0

WebView {
    id: webEngineView
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
}
