## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ArrayGet.java \
  ArrayLength.java \
  ArrayNew.java \
  ArraySet.java \
  ClassMemberConstraint.java \
  ClassMethods.java \
  Invoke.java \
  SlotGet.java \
  SlotSet.java \
  StaticGet.java \
  StaticSet.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

