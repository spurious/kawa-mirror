## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ELispReader.java
java_SCM=\
  streams.scm

KAWA = $(JAVA) kawa.repl

streams.class:  $(java_SCM)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) -P gnu.elisp. -C $?

all: Makefile classjava.stamp streams.class
