## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources=\
  AddOp.java \
  Apply.java \
  Assoc.java \
  DisplayFormat.java \
  IsEq.java IsEqual.java IsEqv.java \
  MakeList.java \
  MakeProcedure.java \
  Member.java \
  Setter.java \
  ObjectFormat.java LispFormat.java ParseFormat.java Format.java 

PACKAGE_FNAME = gnu-kawa-functions
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
