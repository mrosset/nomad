ACLOCAL_AMFLAG = -I m4

libnomad_LTLIBRARIES = libnomad.la
libnomaddir = $(libexecdir)/nomad/
libnomad_la_SOURCES = ext.c
libnomad_la_CFLAGS = $(WEB_EXTENSION_CFLAGS) $(GUILE_CFLAGS)
libnomad_la_LIBADD = $(WEB_EXTENSION_LIBS) $(GUILE_LIBS) $(GLIB_LIBS)
libnomad_la_LDFLAGS = -module -avoid-version -no-undefined0
