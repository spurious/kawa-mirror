## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  ArrayGet.java \
  ArrayLength.java \
  ArrayNew.java \
  ArraySet.java \
  ClassMemberConstraint.java \
  ClassMethods.java \
  InstanceOf.java \
  Invoke.java \
  SlotGet.java \
  SlotSet.java \
  StaticGet.java \
  StaticSet.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-kawa-reflect
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
