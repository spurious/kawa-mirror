## Process this with automake to create Makefile.in

KAWALIB = ../kawa-$(VERSION).jar
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
KAWATEST = $(KAWA) -e '(load "testing.zip")' $(OPTIONS)

all:

check:
	$(MAKE) check2 OPTIONS=--no-full-tailcalls
	$(MAKE) check1
	$(MAKE) check2 OPTIONS=--full-tailcalls

check1:  check-num check-format check-ppfile scribble.class
check2:  check-scm check-misc check-mac check-obj check-polytype

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

check-obj: testing.zip module2.class classes2.class
	$(KAWATEST) -f "$(srcdir)/obj-test.scm" -e '(test-report)'

check-polytype: testing.zip
	$(KAWATEST) -f "$(srcdir)/polytype.scm" -e '(test-report)'

check-format: testing.zip
	$(KAWATEST) \
	  -f "$(srcdir)/formatst.scm" -e '(test-report)'

check-ppfile:
	$(KAWA) -e "(require 'pprint-file)" \
	  -e '(define out (open-output-file "tmpP"))' \
	  -e '(pprint-file "@top_srcdir@/gnu/kawa/slib/ppfile.scm" out)' \
	  -e '(close-output-port out)'
	diff tmpP $(srcdir)/ppfile.out
	rm tmpP

scribble.class: $(srcdir)/scribble.scm
	$(KAWA) --applet -C $(srcdir)/scribble.scm

module1.class: $(srcdir)/module1.scm
	$(KAWA) -C $(srcdir)/module1.scm

module2.class: $(srcdir)/module2.scm module1.class
	$(KAWA) -C $(srcdir)/module2.scm

classes1.class: $(srcdir)/classes1.scm
	$(KAWA) -C $(srcdir)/classes1.scm

SimpleB.class: $(srcdir)/SimpleB.java classes1.class
	CLASSPATH=..:. $(JAVAC) -g $(srcdir)/SimpleB.java -d .

classes2.class: $(srcdir)/classes2.scm SimpleB.class
	$(KAWA) -C $(srcdir)/classes2.scm

APPLETVIEWER = appletviewer

# Not done by "make check" as it is interactive.
check-scribble: scribble.class
	$(APPLETVIEWER) -J-cp -J. -J-cp -J.. file:$(srcdir)/scribble.html

SOURCES = testing.scm test.scm num-test.scm mac-test.scm mac1.scm \
  classes1.scm classes2.scm SimpleB.java obj-test.scm \
  misc-test.scm polytype.scm formatst.scm scribble.scm \
  module1.scm module2.scm
CLEANFILES = tmp? *.log testing.zip *.class
EXTRA_DIST = $(SOURCES) ppfile.out
