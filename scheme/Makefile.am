GOBJECTS = $(SOURCES:%.scm=%.go)
GUILE_WARNINGS = -Wunbound-variable -Wmacro-use-before-definition -Warity-mismatch -Wformat

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_ccache_DATA = $(GOBJECTS)

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
CLEANFILES = $(GOBJECTS)

guile_install_go_files = install-nobase_ccacheDATA
$(guile_install_go_files): install-nobase_modDATA

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

SUFFIXES = .scm .go

test:
	echo $(GUILE_SITE)
	echo $(GUILE_SITE_CCACHE)

.scm.go:
	$(AM_V_GUILEC)GUILE_AUTO_COMPILE=0			\
	$(GUILD) compile --target="$(host)" $(GUILE_WARNINGS)	\
	  -L "$(abs_srcdir)" -L "$(abs_builddir)"		\
	  -o "$@" "$<"

SOURCES =					\
  wemacs/browser.scm				\
  wemacs/events.scm				\
  wemacs/init.scm				\
  wemacs/keymap.scm				\
  wemacs/repl.scm				\
  wemacs/server.scm				\
  wemacs/util.scm
