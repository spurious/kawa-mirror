## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_SCM=\
  $(java_SCM_ALWAYS) \
  $(scm_@AWT_SELECTED@)

# NOTE if you update this list, also update build.xml.
java_SCM_ALWAYS =\
  prim_syntax.scm \
  std_syntax.scm \
  reflection.scm \
  syntax.scm \
  lists.scm \
  thread.scm \
  characters.scm \
  keywords.scm \
  numbers.scm \
  strings.scm \
  parameters.scm \
  ports.scm \
  files.scm \
  misc.scm \
  vectors.scm \
  uniform.scm \
  arrays.scm \
  system.scm \
  trace.scm

scm_WITH_AWT = windows.scm

EXTRA_DIST = $(java_SCM_ALWAYS) $(scm_WITH_AWT)

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)

all: Makefile $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

PACKAGE_FNAME = kawa-lib
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
EXTRA_GCJ_COMPILED = ../../bin/kawa1$(exeext) scm-classes.stamp
EXTRA_GCJ_INPUTS = *.class
SCM_COMPILE_FLAGS = --warn-undefined-variable

