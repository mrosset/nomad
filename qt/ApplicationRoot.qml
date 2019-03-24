import QtQuick 2.1
import QtWebEngine 1.2
import QtQuick.Controls 1.1

QtObject {
    id: root

    property QtObject defaultProfile: WebEngineProfile {
        storageName: "Profile"
        offTheRecord: false
    }

    property QtObject otrProfile: WebEngineProfile {
        offTheRecord: true
    }

    property Component browserWindowComponent: BrowserWindow {
        applicationRoot: root
        onClosing: destroy()
    }

    property Component browserDialogComponent: BrowserDialog {
        onClosing: destroy()
    }
    property WebEngineView currentWebView: window.currentWebView

    property BrowserWindow window

    function createWindow(profile) {
        var newWindow = browserWindowComponent.createObject(root);
        newWindow.currentWebView.profile = profile;
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
        defaultProfile.cachePath = dir + "/cache"
        defaultProfile.persistentStoragePath = dir + "/storage"
    }

    function makeFrame(url) {
        var browserWindow = createWindow(defaultProfile);
        browserWindow.currentWebView.url = url;
    }
}
