## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Average.java \
  BooleanValue.java \
  Children.java \
  Compare.java \
  IntegerRange.java \
  IsEmptySequence.java \
  ItemAt.java \
  MakeAttribute.java \
  MakeElement.java \
  MinMax.java \
  NumberValue.java \
  Position.java \
  Reduce.java \
  StringValue.java \
  SubList.java \
  ValuesFilter.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util
all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
