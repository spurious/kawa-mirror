## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  BRL.java \
  BRLRead.java \
  BRLReaderString.java

PACKAGE_FNAME = gnu-kawa-brl
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
