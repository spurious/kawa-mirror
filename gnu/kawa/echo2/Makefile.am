## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  Echo2AppInstance.java \
  Echo2Display.java \
  Echo2Servlet.java \
  Echo2Window.java

PACKAGE_FNAME = gnu-kawa-echo2
GCJ_INPUTS =  $(java_Java)

if ENABLE_ECHO2
ALL = java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)
