## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  AddOp.java \
  AppendValues.java \
  Apply.java \
  ApplyToArgs.java \
  Arithmetic.java \
  ArithOp.java \
  Arrays.java \
  ArrayRef.java \
  ArraySet.java \
  BitwiseOp.java \
  CallCC.java \
  CompilationHelpers.java \
  CompileArith.java \
  CompileNamedPart.java \
  CompileMisc.java \
  ConstantFunction0.java \
  Convert.java \
  CountValues.java \
  DisplayFormat.java \
  DivideOp.java \
  FileUtils.java \
  GetModuleClass.java \
  GetNamedInstancePart.java \
  GetNamedPart.java \
  IntegerFormat.java \
  IsEq.java IsEqual.java IsEqv.java \
  MakeList.java \
  MakeProcedure.java \
  Map.java \
  MultiplyOp.java \
  Not.java \
  NumberCompare.java \
  NumberPredicate.java \
  SetNamedPart.java \
  Setter.java \
  UnicodeUtils.java \
  ValuesMap.java \
  ObjectFormat.java LispFormat.java ParseFormat.java Format.java 

PACKAGE_FNAME = gnu-kawa-functions
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
