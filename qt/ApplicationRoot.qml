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

    property ApplicationWindow window: ApplicationWindow {

    }

    function createWindow(profile) {
        var newWindow = browserWindowComponent.createObject(root);
        newWindow.currentWebView.profile = profile;
        /* profile.downloadRequested.connect(newWindow.onDownloadRequested); */
        return newWindow;
    }
    function createDialog(profile) {
        var newDialog = browserDialogComponent.createObject(root);
        newDialog.currentWebView.profile = profile;
        return newDialog;
    }

    function load(url) {
        var browserWindow = createWindow(defaultProfile);
        browserWindow.currentWebView.url = url;
        this.window = browserWindow;
    }

    function currentUrl() {
        return this.window.currentUrl();
    }

    function setUrl(url) {
        this.window.setUrl(url);
    }

    function goBack() {
        this.window.goBack();
    }

    function goForward() {
        this.window.goForward();
    }
}
