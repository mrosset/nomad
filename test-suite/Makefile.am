TESTS  = tests/util.scm
TESTS += tests/api.scm
TESTS += tests/init.scm
TESTS += tests/frame.scm
TESTS += tests/application.scm
TESTS += tests/gtk/window.scm

GUILE_AUTO_COMPILE := 0

TEST_EXTENSIONS = .scm

TESTS_ENVIRONMENT = \
	$(abs_top_builddir)/pre-inst-env		\
	$(GUILE) -L $(abs_top_builddir)/test-suite	\
		 --no-auto-compile -s test-runner

test-list:
	echo $(TESTS)

do_subst = sed -e 's,[@]GUILE[@],$(GUILE),g'	\
		-e 's,[@]LIBDIR[@],$(libdir),g' \
		-e 's,[@]CCACHEDIR[@],$(SITECCACHEDIR),g'

all-local: test-runner

CLEANFILES =  test-runner

test-runner: test-runner.in
	$(do_subst) < $< > $@
	chmod +x $@
