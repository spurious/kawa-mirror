KAWALIB = ../../..
KAWA = KAWALIB=$(KAWALIB) $(KAWALIB)/kawa/kawa.sh
DOTEST = $(KAWA) --elisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

SOURCES = lang-test.el

all:

check_WITH_SWING:
	$(DOTEST) -f "$(srcdir)/lang-test.el" -e '(test-report)'

check_WITHOUT_SWING:
	@echo "(no checks, since configued without --with-swing)"

check: check_@SWING_SELECTED@
