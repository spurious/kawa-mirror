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
  UnwindProtect.java \
  defcustom.java defgroup.java \
  lambda.java

java_JAVA=\
  defun.java \
  defvar.java \
  While.java \
  setq.java \
  $(java_@SWING_SELECTED@)

EXTRA_JAVA = $(java_WITH_SWING)

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.lang. -C

SCM_WITH_SWING = SymbolOps.scm NumberOps.scm StringOps.scm ArrayOps.scm \
  MiscOps.scm
SCMSOURCES = ListOps.scm $(SCM_@SWING_SELECTED@)
ALL_SCMSOURCES = ListOps.scm $(SCM_WITH_SWING)

CLEANFILES = *.class *.stamp

EXTRA_DIST = $(ALL_SCMSOURCES) package.html

scmclasses.stamp: $(SCMSOURCES)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) $(KAWAFLAGS) $?
	echo timestamp > scmclasses.stamp

all: Makefile classjava.stamp scmclasses.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(ALL_SCMSOURCES)
