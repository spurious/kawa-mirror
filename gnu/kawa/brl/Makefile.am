## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  BRL.java \
  BRLRead.java \
  BRLReaderString.java

PACKAGE_FNAME = gnu-kawa-brl
GCJ_INPUTS =  $(java_Java)
java_SCM = progfun.scm
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
EXTRA_DIST = $(java_SCM)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)

all: Makefile java-classes.stamp ../../../kawa/java-classes.stamp \
  scm-classes.stamp  $(@GCJ_COMPILED_SELECTED@)
