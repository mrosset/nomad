import QtQuick 2.1
import QtQuick.Controls 1.1
import QtWebKit 3.0

QtObject {
    id: root

    property Component browserWindowComponent: BrowserWindow {
        applicationRoot: root
        onClosing: destroy()
    }

    property WebView currentWebView: window.currentWebView

    property BrowserWindow window

    function createWindow() {
        var newWindow = browserWindowComponent.createObject(root);
        /* newWindow.currentWebView.profile = profile; */
        /* profile.downloadRequested.connect(newWindow.onDownloadRequested); */
        window = newWindow;
        return newWindow;
    }

    function createDialog(profile) {
        var newDialog = browserDialogComponent.createObject(root);
        newDialog.currentWebView.profile = profile;
        return newDialog;
    }

    function setNomadDir(dir) {
        /* defaultProfile.cachePath = dir + "/cache" */
        /* defaultProfile.persistentStoragePath = dir + "/storage" */
    }

    function makeFrame(url) {
        var browserWindow = createWindow();
        browserWindow.currentWebView.url = url;
    }
}
