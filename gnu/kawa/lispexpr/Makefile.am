## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ClassNamespace.java \
  DefineNamespace.java \
  LangPrimType.java \
  LispLanguage.java \
  LispPackage.java \
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
  ReaderTypespec.java \
  ReaderVector.java \
  ReadTable.java \
  ReadTableEntry.java

KAWA = $(JAVA) kawa.repl

PACKAGE_FNAME = gnu-kawa-lispexpr
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
