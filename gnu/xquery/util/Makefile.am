## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Average.java \
  BooleanValue.java \
  Children.java \
  Compare.java \
  Debug.java \
  IndexOf.java \
  IntegerRange.java \
  IsEmptySequence.java \
  ItemAt.java \
  LastIndexOf.java \
  MakeAttribute.java \
  MakeElement.java \
  MinMax.java \
  NumberValue.java \
  Reduce.java \
  RelativeStep.java \
  StringValue.java \
  SubList.java \
  ValuesEvery.java \
  ValuesFilter.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util
all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
