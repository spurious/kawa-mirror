## Process this with automake to create Makefile.in

KAWALIB = ..
KAWA = KAWALIB=$(KAWALIB) ../kawa/kawa.sh
KAWATEST = $(KAWA) -e '(load "testing.zip")' $(OPTIONS)

all:

check:
	$(MAKE) check2 OPTIONS=--no-full-tailcalls
	$(MAKE) check2 OPTIONS=--full-tailcalls
	$(MAKE) check1

check1:  check-num
check2:  check-scm check-misc check-mac check-misc check-obj check-polytype

testing.zip: $(srcdir)/testing.scm
	$(KAWA) -e '(compile-file "'$(srcdir)/testing.scm'" "testing.zip")'

check-scm: testing.zip
	rm -rf tmp?
	$(KAWATEST) \
	  -e '(define this-file-name "$(srcdir)/test.scm")' \
	  -e "(set! symbol-read-case 'D)" \
	  -f $(srcdir)/test.scm -e '(test-report)'

check-num: testing.zip
	$(KAWATEST) -f "$(srcdir)/num-test.scm" -e '(test-report)'

check-misc: testing.zip
	$(KAWATEST) -f "$(srcdir)/misc-test.scm" -e '(test-report)'

check-mac: testing.zip
	$(KAWATEST) -e '(define src-prefix "$(srcdir)/")' \
	  -f "$(srcdir)/mac-test.scm" -e '(test-report)'

check-obj: testing.zip
	$(KAWATEST) -f "$(srcdir)/obj-test.scm" -e '(test-report)'

check-polytype: testing.zip
	$(KAWATEST) -f "$(srcdir)/polytype.scm" -e '(test-report)'

SOURCES = testing.scm test.scm num-test.scm mac-test.scm mac1.scm \
  obj-test.scm misc-test.scm polytype.scm
CLEANFILES = tmp? *.log testing.zip
