## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_WITH_SWING =\
  ELisp.java \
  ELispReader.java \
  AddOp.java \
  DivideOp.java \
  Print.java \
  NumberCompare.java \
  SaveExcursion.java \
  defcustom.java defgroup.java \
  lambda.java

java_sources=\
  While.java \
  $(java_@SWING_SELECTED@)

java_SCM = MiscOps.scm NumberOps.scm
PACKAGE_FNAME = gnu-jemacs-lang

EXTRA_DIST = $(java_WITH_SWING) $(java_SCM) package.html
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

all: Makefile java-classes.stamp scm-classes.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(ALL_SCMSOURCES)
