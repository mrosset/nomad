import QtQuick 2.11
import QtQuick.Layouts 1.11

Rectangle {
    height: 200
    visible: false
    Layout.fillWidth: true
    color: "white"
    ListView {
	objectName: "miniPopup"
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
	function handleUpdateMap(bind, proc) {
	    miniPopupModel.append({"bind": bind, "symbol": proc})
	}
    }
    onFocusChanged: {
	visible = focus
	miniPopupModel.clear()
    }
    onVisibleChanged: {
	miniSeperator.visible = visible
	focus = visible
	miniBufferLabel.visible = visible
	if(visible) {
	    miniBuffer.prompt = "C-x"
	} else {
	    miniBuffer.prompt = "M-x"
	}
	updateMap("ctrl-x-map");
    }
    Keys.onPressed: {
	submitKeymap("ctrl-x-map", event.modifiers, event.key)
	keyboardQuit();
    }
    ListModel {
	id: miniPopupModel
	ListElement {
	    bind: ""
	    symbol: ""
	}
    }
}
