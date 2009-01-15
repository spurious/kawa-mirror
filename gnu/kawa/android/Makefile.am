## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=

java_SCM = defs.scm

EXTRA_DIST = defs.scm
GCJ_INPUTS = defs*.class
EXTRA_GCJ_DEPS = scm-classes.stamp
PACKAGE_FNAME = gnu-kawa-android

all: Makefile scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
