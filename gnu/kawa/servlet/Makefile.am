## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources=\
  KawaServlet.java

PACKAGE_FNAME = gnu-kawa-servlet
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
