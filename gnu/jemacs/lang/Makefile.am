## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

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

java_sources=\
  defun.java \
  defvar.java \
  While.java \
  setq.java \
  $(java_@SWING_SELECTED@)

SCM_WITH_SWING = SymbolOps.scm NumberOps.scm StringOps.scm ArrayOps.scm \
  MiscOps.scm
java_SCM = ListOps.scm $(SCM_@SWING_SELECTED@)
PACKAGE_FNAME = gnu-jemacs-lang
ALL_SCMSOURCES = ListOps.scm $(SCM_WITH_SWING)

EXTRA_DIST = $(java_WITH_SWING) $(ALL_SCMSOURCES) package.html
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

all: Makefile java-classes.stamp scm-classes.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(ALL_SCMSOURCES)
