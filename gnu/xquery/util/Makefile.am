## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ArithOp.java \
  Average.java \
  BooleanValue.java \
  CastableAs.java \
  CastAs.java \
  Compare.java \
  Debug.java \
  DistinctValues.java \
  IntegerRange.java \
  ItemAt.java \
  MinMax.java \
  NamedCollator.java \
  NodeUtils.java \
  NumberValue.java \
  OrderedMap.java \
  OrderedTuples.java \
  QNameUtils.java \
  Reduce.java \
  RelativeStep.java \
  SequenceUtils.java \
  StringUtils.java \
  SubList.java \
  TimeUtils.java \
  ValuesEvery.java \
  ValuesFilter.java \
  XMLFormat.java

PACKAGE_FNAME = gnu-xquery-util
all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
