## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  random.java

PACKAGE_FNAME = gnu-brl
GCJ_INPUTS =  $(java_Java)
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

java_SCM = stringfun.scm
EXTRA_DIST = $(java_SCM)

all: Makefile java-classes.stamp scm-classes.stamp  $(@GCJ_COMPILED_SELECTED@)
