## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  ConsumeProc.java \
  AbstractFormat.java \
  RangeTable.java

PACKAGE_FNAME = gnu-kawa-util
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
