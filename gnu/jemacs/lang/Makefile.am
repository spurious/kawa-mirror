## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  ELisp.java \
  ELispReader.java \
  ObArray.java \
  Symbol.java \
  NumberCompare.java \
  defun.java \
  lambda.java \
  setq.java

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.lang. -C

SCMSOURCES = SymbolOps.scm NumberOps.scm

CLEANFILES = *.class *.stamp

scmclasses.stamp: $(SCMSOURCES)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) $(KAWAFLAGS) $?
	echo timestamp > scmclasses.stamp

all: Makefile classjava.stamp scmclasses.stamp
