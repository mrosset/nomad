AC_INIT(wemacs.c)

# Find a C compiler.
AC_PROG_CC

# Check for Guile
PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([GTK], [gtk+-3.0])

# Generate a Makefile, based on the results.
AC_OUTPUT(Makefile.gnu)
