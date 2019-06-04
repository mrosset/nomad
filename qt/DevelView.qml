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
