## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
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

KAWA = $(JAVA) kawa.repl

PACKAGE_FNAME = gnu-kawa-reflect

all: Makefile classjava.stamp $(@GCJ_COMPILED_SELECTED@)
