## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  AddOp.java \
  Apply.java \
  Assoc.java \
  IsEq.java IsEqual.java IsEqv.java \
  MakeList.java \
  MakeProcedure.java \
  Member.java \
  Setter.java \
  ObjectFormat.java LispFormat.java ParseFormat.java Format.java 

PACKAGE_FNAME = gnu-kawa-functions

all: Makefile classjava.stamp $(@GCJ_COMPILED_SELECTED@)
