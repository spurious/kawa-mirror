## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_SCM=\
  std_syntax.scm \
  reflection.scm \
  syntax.scm \
  lists.scm \
  thread.scm \
  characters.scm \
  files.scm \
  keywords.scm \
  ports.scm \
  misc.scm \
  numbers.scm \
  strings.scm \
  vectors.scm \
  uniform.scm \
  system.scm \
  trace.scm

EXTRA_DIST = $(java_SCM)

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)

all: Makefile $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

PACKAGE_FNAME = kawa-lib
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
ENABLE_GCJ_COMPILED = ../../bin/kawa1$(exeext) scm-classes.stamp $(OFILES_DIR)/$(PACKAGE_FNAME).o
EXTRA_GCJ_INPUTS = *.class
