## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_WITH_SWING =\
  ELisp.java \
  ELispReader.java \
  AddOp.java \
  DivideOp.java \
  ObArray.java \
  Symbol.java \
  NumberCompare.java \
  SaveExcursion.java \
  defun.java \
  defvar.java \
  lambda.java \
  setq.java

java_JAVA=\
  While.java \
  $(java_@SWING_SELECTED@)

EXTRA_JAVA = $(java_WITH_SWING)

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.lang. -C

SCMSOURCES = SymbolOps.scm NumberOps.scm StringOps.scm ArrayOps.scm \
  ListOps.scm MiscOps.scm

CLEANFILES = *.class *.stamp

EXTRA_DIST = $(SCMSOURCES) package.html

scmclasses.stamp: $(SCMSOURCES)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) $(KAWAFLAGS) $?
	echo timestamp > scmclasses.stamp

all: Makefile classjava.stamp scmclasses.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
