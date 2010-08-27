## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ArrayGet.java \
  ArrayLength.java \
  ArrayNew.java \
  ArraySet.java \
  ClassMemberLocation.java \
  ClassMethods.java \
  CompileArrays.java \
  CompileInvoke.java \
  CompileReflect.java \
  FieldLocation.java \
  InstanceOf.java \
  Invoke.java \
  OccurrenceType.java \
  SingletonType.java \
  SlotGet.java \
  SlotSet.java \
  StaticFieldLocation.java \
  StaticGet.java \
  StaticSet.java \
  TypeSwitch.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-kawa-reflect
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
