## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources=\
  AddOp.java \
  Apply.java \
  Assoc.java \
  ConstantFunction0.java \
  DisplayFormat.java \
  DivideOp.java \
  IsEq.java IsEqual.java IsEqv.java \
  MakeList.java \
  MakeProcedure.java \
  MultiplyOp.java \
  NumberCompare.java \
  Member.java \
  Setter.java \
  ValuesMap.java \
  ObjectFormat.java LispFormat.java ParseFormat.java Format.java 

PACKAGE_FNAME = gnu-kawa-functions
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
