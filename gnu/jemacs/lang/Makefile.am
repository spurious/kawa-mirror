## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

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

SCM_WITH_SWING = SymbolOps.scm NumberOps.scm StringOps.scm ArrayOps.scm \
  MiscOps.scm
java_SCM = ListOps.scm $(SCM_@SWING_SELECTED@)
PACKAGE_CFNAME = gnu-jemacs-lang
ALL_SCMSOURCES = ListOps.scm $(SCM_WITH_SWING)

EXTRA_DIST = $(ALL_SCMSOURCES) $(EXTRA_JAVA) package.html
CLEANFILES = *.class *.stamp *.jar

all: Makefile classjava.stamp scm-classes.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(ALL_SCMSOURCES)
