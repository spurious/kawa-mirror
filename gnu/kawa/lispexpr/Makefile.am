## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
  LispReader.java \
  ReaderDispatch.java \
  ReaderDispatchMacro.java \
  ReaderDispatchMisc.java \
  ReaderIgnoreRestOfLine.java \
  ReaderMacro.java \
  ReaderMisc.java \
  ReaderParens.java \
  ReaderQuote.java \
  ReaderString.java \
  ReaderVector.java \
  ReadTable.java \
  ReadTableEntry.java \
  ScmRead.java

KAWA = $(JAVA) kawa.repl

PACKAGE_FNAME = gnu-kawa-lispexpr

all: Makefile classjava.stamp $(@GCJ_COMPILED_SELECTED@)
