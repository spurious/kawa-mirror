## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  AppendValues.java \
  Children.java \
  Compare.java \
  Document.java \
  ElementType.java \
  IntegerRange.java \
  IsEmptySequence.java \
  ItemAt.java \
  MakeAttribute.java \
  MakeElement.java \
  NamedChildren.java \
  NodeType.java \
  StringValue.java \
  SubList.java \
  ValuesFilter.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
