## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources = \
  Attributes.java \
  ElementConstructor.java \
  NamedAttributes.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
