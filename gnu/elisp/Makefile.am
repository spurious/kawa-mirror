## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ELisp.java \
  ELispReader.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

