## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  LangPrimType.java \
  LispInterpreter.java \
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
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
