## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
  AbstractString.java \
  CharBuffer.java \
  Consumable.java \
  ConsumeProc.java \
  Consumer.java \
  ConstantString.java \
  F32Vector.java \
  F64Vector.java \
  FString.java \
  FVector.java \
  LList.java \
  Pair.java \
  PairWithPosition.java \
  Position.java \
  S8Vector.java \
  S16Vector.java \
  S32Vector.java \
  S64Vector.java \
  Sequence.java \
  SubString.java \
  TreeList.java \
  U8Vector.java \
  U16Vector.java \
  U32Vector.java \
  U64Vector.java \
  UniformVector.java

KAWA = $(JAVA) kawa.repl

PACKAGE_FNAME = gnu-kawa-util

all: Makefile classjava.stamp $(@GCJ_COMPILED_SELECTED@)
