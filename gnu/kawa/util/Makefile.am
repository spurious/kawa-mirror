## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  AbstractString.java \
  Char.java \
  CharBuffer.java \
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
  U8Vector.java \
  U16Vector.java \
  U32Vector.java \
  U64Vector.java \
  UniformVector.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

