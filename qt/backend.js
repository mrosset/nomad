var keymap;

window.onload = function()
{
    new QWebChannel(qt.webChannelTransport, function(channel) {
	keymap = channel.objects.keymap;
    });
}
