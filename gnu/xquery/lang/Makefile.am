## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  XQuery.java \
  XQParser.java

PACKAGE_FNAME = gnu-xquery-lang

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
