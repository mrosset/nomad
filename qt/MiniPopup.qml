import QtQuick 2.11
import QtQuick.Layouts 1.11
import QtQuick.Controls 1.2

Rectangle {
    visible: true
    height: 200
    width: parent.width
    property string keymap: null
    Component.onCompleted: {
	parent.visible = true
	focus = true
    }
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
    }
    function handleUpdateMap(bind, proc) {
	miniPopupModel.append({"bind": bind, "symbol": proc})
    }
    onFocusChanged: {
	miniPopupModel.clear()
    }
    onVisibleChanged: {
	miniSeperator.visible = visible
	miniBufferLabel.visible = visible
    }
    Keys.onPressed: {
	submitKeymap(keymap, event.modifiers, event.key)
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
