## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

PACKAGE_CFNAME = kawa-lib
ENABLE_GCJ_COMPILED = ../../bin/kawa1$(exeext) scm-classes.stamp $(OFILES_DIR)/$(PACKAGE_CFNAME).o

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

java_JAVA =

CLEANFILES = *.class *.stamp $(OFILES_DIR)/$(PACKAGE_CFNAME).o

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)

classjava.stamp: scm-classes.stamp
	touch classjava.stamp

all: Makefile $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
