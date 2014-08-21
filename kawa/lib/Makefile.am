## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

# NOTE if you update this list, also update build.xml.
java_SCM_ALWAYS =\
  prim_syntax.scm \
  std_syntax.scm \
  reflection.scm \
  syntax.scm \
  lists.scm \
  case_syntax.scm \
  ExceptionClasses.scm \
  exceptions.scm \
  thread.scm \
  characters.scm \
  keywords.scm \
  numbers.scm \
  strings.scm \
  parameters.scm \
  parameterize.scm \
  ports.scm \
  files.scm \
  misc.scm \
  misc_syntax.scm \
  vectors.scm \
  uniform.scm \
  bytevectors.scm \
  arrays.scm \
  system.scm \
  kawa/expressions.scm \
  kawa/string-cursors.scm \
  kawa/hashtable.scm kawa/regex.scm rnrs/hashtables.scm rnrs/lists.scm \
  srfi95.scm rnrs/unicode.scm rnrs/sorting.scm rnrs/programs.scm \
  compile_misc.scm compile_map.scm \
  trace.scm

scm_WITH_SWING = windows.scm
EXTRA_DIST = $(java_SCM_ALWAYS) $(scm_WITH_SWING)

if WITH_SWING
java_SCM= $(java_SCM_ALWAYS) $(scm_WITH_SWING)
else
java_SCM= $(java_SCM_ALWAYS)
endif

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)

all: Makefile scm-classes.stamp

PACKAGE_FNAME = kawa-lib
SCM_COMPILE_FLAGS = --warn-undefined-variable --warn-unknown-member --warn-as-error
EXTRA_CLEAN = */*.class
