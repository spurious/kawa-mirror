## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ELisp.java \
  ELispReader.java \
  FunctionSymbolConstraint.java \
  GeneralSymbolConstraint.java \
  ObArray.java \
  Symbol.java \
  SymbolConstraint.java \
  UnboundSymbolConstraint.java \
  ValueSymbolConstraint.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

