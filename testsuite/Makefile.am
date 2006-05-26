## Process this with automake to create Makefile.in

KAWALIB = ../kawa-$(VERSION).jar
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
KAWATEST = CLASSPATH=.:$$CLASSPATH $(KAWA) -e '(load "testing.zip")' $(OPTIONS)

if ENABLE_SERVLET
SERVLET_TESTS = check-HelloCgiS check-HelloCgiK check-HelloCgiX
endif
if WITH_SAX2
SAX2_TESTS = check-MySAXApp
endif
all:

check: check2-no-full-tailcalls check1 check2-full-tailcalls check-no-inline \
  $(SERVLET_TESTS) $(SAX2_TESTS) check-inlining check-use-slots

check2-no-full-tailcalls:
	$(MAKE) check2 OPTIONS=--no-full-tailcalls

check2-full-tailcalls:
	$(MAKE) check2 OPTIONS=--full-tailcalls

check-no-inline:
	$(MAKE) check2 OPTIONS=--no-inline

check1:  check-num check-arr check-srfi-64 check-format \
  check-ppfile scribble.class
check2:  check-scm check-misc check-mac check-obj check-polytype

testing.zip: $(srcdir)/testing.scm
	$(KAWA) -e '(compile-file "'$(srcdir)/testing.scm'" "testing.zip")'

check-scm: testing.zip
	rm -rf tmp?
	$(KAWATEST) \
	  -e '(define this-file-name "$(srcdir)/test.scm")' \
	  -e "(set! symbol-read-case 'D)" \
	  -f $(srcdir)/test.scm -e '(test-report)'

check-num:
	$(KAWA) $(OPTIONS) -f "$(srcdir)/num-test.scm"

check-arr:
	$(KAWA) $(OPTIONS) -f "$(srcdir)/arr-test.scm"

check-srfi-64:
	$(KAWA) $(OPTIONS) -f "$(srcdir)/srfi-64-test.scm"

check-misc: testing.zip moduleFT.class
	$(KAWATEST) -f "$(srcdir)/misc-test.scm" -e '(test-report)'

check-mac: testing.zip
	$(KAWATEST) -e '(define src-prefix "$(srcdir)/")' \
	  -f "$(srcdir)/mac-test.scm" -e '(test-report)'

check-obj: testing.zip module2.class module3.class classes2.class \
  MyFunc.class MyModule.class cycle1.class cycle2.class
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

moduleFT.class: $(srcdir)/moduleFT.scm
	CLASSPATH=.:$$CLASSPATH $(KAWA) --full-tailcalls -C $(srcdir)/moduleFT.scm

module0.class: $(srcdir)/module0.scm
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/module0.scm

module1.class: $(srcdir)/module1.scm module0.class
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/module1.scm

module1a.class: $(srcdir)/module1a.scm
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/module1a.scm

module2.class: $(srcdir)/module2.scm module1.class module1a.class
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/module2.scm

module3.class: $(srcdir)/module3.scm module1.class MyTimestamp.class
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/module3.scm

classes1.class: $(srcdir)/classes1.scm
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $(srcdir)/classes1.scm

SimpleB.class: $(srcdir)/SimpleB.java classes1.class
	CLASSPATH=..:.:$$CLASSPATH $(JAVAC) -g $(srcdir)/SimpleB.java -d .

classes2.class: $(srcdir)/classes2.scm SimpleB.class
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	$(KAWA) -C $(srcdir)/classes2.scm

HelloCgiK.class: $(srcdir)/HelloCgiK.krl
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	$(KAWA) --servlet --krl -C $(srcdir)/HelloCgiK.krl

HelloCgiK:
	$(LN_S) ../bin/cgi-servlet HelloCgiK

check-HelloCgiK: HelloCgiK.class HelloCgiK
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	REQUEST_METHOD=GET SCRIPT_NAME=HelloCgiK SERVER_NAME=localhost \
	  KAWALIB=$(KAWALIB) ./HelloCgiK >tmp.out
	@if diff -b $(srcdir)/HelloCgiK.expected tmp.out; \
	  then echo '# HelloCgiK test passes'; \
	  else echo FAIL HelloCgiK test; fi

HelloCgiS.class: $(srcdir)/HelloCgiS.scm
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	$(KAWA) --servlet --output-format xml -C $(srcdir)/HelloCgiS.scm

HelloCgiS:
	$(LN_S) ../bin/cgi-servlet HelloCgiS

check-HelloCgiS: HelloCgiS.class HelloCgiS
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	REQUEST_METHOD=GET SCRIPT_NAME=HelloCgiS SERVER_NAME=localhost \
	  KAWALIB=$(KAWALIB) ./HelloCgiS >tmp.out
	@if diff -b $(srcdir)/HelloCgiS.expected tmp.out; \
	  then echo '# HelloCgiS test passes'; \
	  else echo FAIL HelloCgiS test; fi

HelloCgiX.class: $(srcdir)/HelloCgiX.xql
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	$(KAWA) --servlet -C $(srcdir)/HelloCgiX.xql

HelloCgiX:
	$(LN_S) ../bin/cgi-servlet HelloCgiX

check-HelloCgiX: HelloCgiX.class HelloCgiX
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	REQUEST_METHOD=GET SCRIPT_NAME=HelloCgiX SERVER_NAME=localhost \
	  PATH_TRANSLATED=./HelloCgiX KAWALIB=$(KAWALIB) ./HelloCgiX >tmp.out
	@if diff -b $(srcdir)/HelloCgiX.expected tmp.out; \
	  then echo '# HelloCgiX test passes'; \
	  else echo FAIL HelloCgiX test; fi

MySAXApp.class: $(srcdir)/MySAXApp.java
	CLASSPATH=..:.:$$CLASSPATH $(JAVAC) -g $(srcdir)/MySAXApp.java -d .

check-MySAXApp: MySAXApp.class
	CLASSPATH=..:.:$$CLASSPATH $(JAVA) \
	  -Dorg.xml.sax.driver=gnu.kawa.sax.KawaXMLReader\
	  MySAXApp ../gnu/xquery/testsuite/reviews.xml > tmp.out
	@if diff -b $(srcdir)/MySAXApp.expected tmp.out; \
	  then echo '# MySAXApp test passes'; \
	  else echo FAIL MySAXApp test; fi

MyFunc.class MyModule.class: $(srcdir)/MyFunc.java $(srcdir)/MyModule.java
	CLASSPATH=..:.:$$CLASSPATH $(JAVAC) -g -d . $?

MyDate.class MyTimestamp.class: $(srcdir)/MyDate.java $(srcdir)/MyTimestamp.java
	CLASSPATH=..:.:$$CLASSPATH $(JAVAC) -g -d . $?

check-pitfalls:
	CLASSPATH=.:$$CLASSPATH $(KAWA) -f $(srcdir)/r5rs_pitfall.scm

APPLETVIEWER = appletviewer

# Not done by "make check" as it is interactive.
check-scribble: scribble.class
	$(APPLETVIEWER) -J-cp -J. -J-cp -J.. scribble.html

cycle1.class cycle2.class: $(srcdir)/cycle1.scm $(srcdir)/cycle2.scm
	CLASSPATH=.:$$CLASSPATH $(KAWA) -C $^

InliningTest.class: $(srcdir)/inlining-test.scm
	CLASSPATH=@conf_classpath@.:$$CLASSPATH \
	$(KAWA) -T InliningTest -C $(srcdir)/inlining-test.scm

inlining.log: InliningTest.class cycle1.class cycle2.class Makefile
	CLASSPATH=..:@conf_classpath@.:$$CLASSPATH \
	$(JAVA) gnu.bytecode.ListCodeSize InliningTest.class \
	setCar getHome setHome isPair isEmpty castToPair newPair tohex \
	makeIarr lengthIarr getIarr setIarr >inlining.log
	CLASSPATH=..:@conf_classpath@.:$$CLASSPATH \
	$(JAVA) gnu.bytecode.ListCodeSize cycle1.class isIsEven >>inlining.log
	CLASSPATH=..:@conf_classpath@.:$$CLASSPATH \
	$(JAVA) gnu.bytecode.ListCodeSize cycle2.class isIsOdd >>inlining.log

check-inlining: inlining.log $(srcdir)/inlining.expected
	@if diff $(srcdir)/inlining.expected inlining.log; \
	  then echo '# inlining test passes'; \
	  else echo FAIL inlining test; fi

Base.class: $(srcdir)/Base.java
	CLASSPATH=..:.:$$CLASSPATH $(JAVAC) -g $< -d .

ASupport.class: $(srcdir)/ASupport.scm Base.class
	CLASSPATH=@conf_classpath@.:$$CLASSPATH $(KAWA) -C $<

check-use-slots: ASupport.class
	$(KAWA) $(OPTIONS) -f $(srcdir)/use-slots.scm

CLEANFILES = tmp? *.log testing.zip *.class HelloCgiK HelloCgiS
EXTRA_DIST = testing.scm test.scm num-test.scm mac-test.scm mac1.scm \
  classes1.scm classes2.scm SimpleB.java obj-test.scm arr-test.scm \
  MyFunc.java MyModule.java MyDate.java MyTimestamp.java \
  misc-test.scm polytype.scm formatst.scm scribble.scm moduleFT.scm \
  module0.scm module1.scm module1a.scm module2.scm module3.scm included-1.scm \
  HelloCgiK.krl HelloCgiS.scm HelloCgiX.xql \
  ppfile.out scribble.html.in \
  MySAXApp.java MySAXApp.expected r5rs_pitfall.scm \
  HelloCgiK.expected HelloCgiS.expected HelloCgiX.expected \
  inlining-test.scm inlining.expected cycle1.scm cycle2.scm
