## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  SwtDisplay.java

PACKAGE_FNAME = gnu-kawa-swtviews
GCJ_INPUTS =  $(java_Java)

if WITH_SWT
ALL = java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)
