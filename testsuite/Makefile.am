## Process this with automake to create Makefile.in

JAVA=java
JAVAFLAGS=-classpath ..:$$CLASSPATH
#KAWA = $(JAVA) $(JAVAFLAGS) kawa.repl
KAWA = KAWALIB=.. ../kawa/kawa
KAWATEST = $(JAVA) $(JAVAFLAGS) kawa.repl -f "$(srcdir)/testing.scm"
KAWATEST = $(KAWA) -f "$(srcdir)/testing.scm"

MISC_1_1 = check-obj

all:

check:  check-scm check-num check-mac check-misc @EXTRA_MISC@

check-scm:
	rm -rf tmp?
	$(KAWATEST) -e '(define this-file-name "$(srcdir)/test.scm")' \
	  -f $(srcdir)/test.scm -e '(test-report)'

check-num:
	$(KAWATEST) -f "$(srcdir)/num-test.scm" -e '(test-report)'

check-misc:
	$(KAWATEST) -f "$(srcdir)/misc-test.scm" -e '(test-report)'

check-mac:
	$(KAWATEST) -e '(define src-prefix "$(srcdir)/")' \
	  -f "$(srcdir)/mac-test.scm" -e '(test-report)'

check-obj:
	$(KAWATEST) -f "$(srcdir)/obj-test.scm" -e '(test-report)'

SOURCES = testing.scm test.scm num-test.scm mac-test.scm mac1.scm \
  obj-test.scm misc-test.scm
CLEANFILES = tmp?
