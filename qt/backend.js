var nomad_backend;

window.onload = function()
{
    new QWebChannel(qt.webChannelTransport, function(channel) {
	// all published objects are available in channel.objects under
	// the identifier set in their attached WebChannel.id property
	nomad_backend = channel.objects.backend;

	// connect to a signal
	nomad_backend.hints.connect(function() {
	    // document.getElementById("lbl").innerHTML = someText;
	});
    });
}
