## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

if ENABLE_XML
XML_JSOURCES = \
  MakeXmlElement.java \
  ReaderXmlElement.java \
  ResolveNamespace.java
endif

java_sources = \
  ClassNamespace.java \
  DefineNamespace.java \
  LangObjType.java \
  LangPrimType.java \
  LispLanguage.java \
  LispPackage.java \
  LispReader.java \
  ReaderColon.java \
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
  ReadTableEntry.java \
  $(XML_JSOURCES)

KAWA = $(JAVA) kawa.repl

PACKAGE_FNAME = gnu-kawa-lispexpr
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
