## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_sources = \
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
  StringValue.java \
  SubList.java \
  ValuesFilter.java \
  XMLFormat.java

java_BUILT = \
  Focus.java

PACKAGE_FNAME = gnu-xquery-util

BUILT_SOURCES = $(java_BUILT)
EXTRA_DIST = Focus.java.in

Focus.java:  $(srcdir)/Focus.java.in
	sed $(DONT_EDIT_SUBS) $(SED_REFERENCES_SUBS) \
	  <$(srcdir)/Focus.java.in >Focus.java

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
