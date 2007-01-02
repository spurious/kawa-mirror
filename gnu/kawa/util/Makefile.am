## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  FixupHtmlToc.java \
  GeneralHashTable.java \
  HashNode.java \
  RangeTable.java

EXTRA_DIST = PreProcess.java
PACKAGE_FNAME = gnu-kawa-util
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
