## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  AppendValues.java \
  Document.java \
  MakeAttribute.java \
  MakeElement.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
