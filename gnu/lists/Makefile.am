## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  AbstractFormat.java \
  AbstractSequence.java \
  Array.java \
  AttributePredicate.java \
  BitVector.java \
  ByteVector.java \
  CharBuffer.java \
  CharSeq.java \
  Consumer.java \
  ConsumerWriter.java \
  Consumable.java \
  Convert.java \
  ElementPredicate.java \
  EofClass.java \
  ExtPosition.java \
  ExtSequence.java \
  F32Vector.java \
  F64Vector.java \
  FilterConsumer.java \
  FString.java \
  FVector.java \
  GapVector.java \
  GeneralArray.java \
  GeneralArray1.java \
  ItemPredicate.java \
  ImmutablePair.java \
  LList.java \
  LListPosition.java \
  NodePredicate.java \
  Pair.java \
  PairWithPosition.java \
  PositionConsumer.java \
  PositionManager.java \
  PrintConsumer.java \
  S8Vector.java \
  S16Vector.java \
  S32Vector.java \
  S64Vector.java \
  SeqPosition.java \
  Sequence.java \
  SimpleVector.java \
  SubCharSeq.java \
  SubSequence.java \
  StableVector.java \
  Strings.java \
  TreeList.java \
  TreePosition.java \
  U8Vector.java \
  U16Vector.java \
  U32Vector.java \
  U64Vector.java \
  UnescapedData.java \
  VoidConsumer.java \
  XConsumer.java

PACKAGE_FNAME = gnu-lists
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
