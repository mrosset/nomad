import QtQuick 2.2
import QtQuick.Window 2.1
import QtWebEngine 1.7

Window {
    width: 1024
    height: 750
    visible: true
    WebEngineView {
        anchors.fill: parent
        url: "https://google.ca"
    }
}
