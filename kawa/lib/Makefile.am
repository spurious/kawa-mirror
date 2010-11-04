## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

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
  misc_syntax.scm \
  vectors.scm \
  uniform.scm \
  arrays.scm \
  system.scm \
  kawa/hashtable.scm kawa/regex.scm rnrs/hashtables.scm \
  srfi95.scm rnrs/unicode.scm rnrs/sorting.scm rnrs/programs.scm \
  trace.scm

scm_WITH_AWT = windows.scm
EXTRA_DIST = $(java_SCM_ALWAYS) $(scm_WITH_AWT)

if WITH_AWT
java_SCM= $(java_SCM_ALWAYS) $(scm_WITH_AWT)
else
java_SCM= $(java_SCM_ALWAYS)
endif

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)

all: Makefile $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

PACKAGE_FNAME = kawa-lib
EXTRA_GCJ_COMPILED = scm-classes.stamp
EXTRA_GCJ_INPUTS = *.class
SCM_COMPILE_FLAGS = --warn-undefined-variable --warn-unknown-member --warn-as-error
EXTRA_CLEAN = */*.class


