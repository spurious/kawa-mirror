## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  Children.java \
  Compare.java \
  IntegerRange.java \
  IsEmptySequence.java \
  ItemAt.java \
  MakeAttribute.java \
  MakeElement.java \
  NumberValue.java \
  StringValue.java \
  SubList.java \
  ValuesFilter.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
