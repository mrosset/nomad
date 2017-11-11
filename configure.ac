AC_INIT([wemacs], [0.0.1], [mike.rosset@gmail.com])

# Find a C compiler.
AC_PROG_CC

# Check for Guile
PKG_CHECK_MODULES([guile], [guile-2.2])
PKG_CHECK_MODULES([gtk], [gtk+-3.0])
PKG_CHECK_MODULES([webkit], [webkit2gtk-4.0])
PKG_CHECK_MODULES([vte], [vte-2.91])

# Generate a Makefile, based on the results.
AC_OUTPUT(Makefile)
