## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Attributes.java \
  AttributeConstructor.java \
  DefineNamespace.java \
  Document.java \
  ElementConstructor.java \
  ElementType.java \
  HttpPrinter.java \
  MakeResponseHeader.java \
  MakeUnescapedData.java \
  NamedAttributes.java \
  NamedChildren.java \
  NamedDescendants.java \
  NamespaceEnv.java \
  NodeType.java \
  OutputAsXML.java \
  WriteTo.java

java_BUILT = \
  Focus.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)
BUILT_SOURCES = $(java_BUILT)
EXTRA_DIST = Focus.java.in

Focus.java:  $(srcdir)/Focus.java.in
	sed $(DONT_EDIT_SUBS) $(SED_REFERENCES_SUBS) \
	  <$(srcdir)/Focus.java.in >Focus.java

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
