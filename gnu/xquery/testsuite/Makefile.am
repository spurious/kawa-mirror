KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
CLEANFILES = tmp? *.log testing.zip *.class

SOURCES = test1.xql expected1.xml

all:

test1.class:  test1.xql
	$(KAWA) --main --xquery -C $<

check-test1:  test1.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) test1 >tmp1
	diff -b $(srcdir)/expected1.xml tmp1

check:  check-test1
