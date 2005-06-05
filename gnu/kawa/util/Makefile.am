## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ConsumeProc.java \
  RangeTable.java

PACKAGE_FNAME = gnu-kawa-util
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
