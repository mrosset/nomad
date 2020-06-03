TESTS  = tests/util.scm
TESTS += tests/init.scm
TESTS += tests/buffer.scm
TESTS += tests/gtk/widget.scm

# TESTS += tests/frame.scm
# TESTS += tests/application.scm
# TESTS += tests/gtk/window.scm
# TESTS += tests/gtk/web.scm
# TESTS += tests/gtk/text.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER  = $(abs_top_builddir)/pre-inst-env \
		    $(GUILE) --debug -L "$(abs_top_builddir)/test-suite"

SCM_LOG_DRIVER = $(abs_top_builddir)/tools/test-driver

AM_SCM_LOG_FLAGS = -s test-runner
AM_LOG_FLAGS = --color-tests=yes
AM_TESTS_ENVIRONMENT = GUILE_LOAD_PATH="$(abs_top_builddir)/test-suite"

# TESTS_ENVIRONMENT = GUILE_LOAD_PATH="$(abs_top_builddir)/test-suite" \
#		    $(abs_top_builddir)/pre-inst-env ./test-runner

test-list:
	echo $(TESTS)

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g'	\
		-e 's,[@]LIBDIR[@],$(libdir),g' \
		-e 's,[@]CCACHEDIR[@],$(SITECCACHEDIR),g'

all-local: test-runner

EXTRA_DIST = test-runner.in $(TESTS)
CLEANFILES = test-runner

test-runner: test-runner.in
	$(do_subst) < $< > $@
	chmod +x $@
