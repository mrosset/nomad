import QtQuick 2.11
import QtQuick.Layouts 1.11

Rectangle {
    height: 200
    visible: false
    Layout.fillWidth: true
    color: "white"
    ListView {
	anchors.fill: parent
	model: miniPopupModel
	delegate:
	RowLayout {
	    Text {
		text: bind
		color: "steelblue"
	    }
	    Text {
		width: parent.width
		text: symbol
	    }
	}
    }
    onFocusChanged: {
	visible = focus
    }
    onVisibleChanged: {
	miniSeperator.visible = visible
	focus = visible
	if(visible) {
	    miniPopupModel.append({"bind": "C-c", "symbol": "(kill-buffer)"})
	}
    }
    Keys.onPressed: {
	submitKeymap("ctrl-x-map", event.modifiers, event.key)
    }
    ListModel {
	id: miniPopupModel
	ListElement {
	    bind: ""
	    symbol: ""
	}
    }
}
