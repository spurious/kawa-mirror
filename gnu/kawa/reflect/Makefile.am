## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ClassMethods.java \
  InvokeStatic.java \
  MakeInstance.java \
  SlotGet.java \
  SlotSet.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

