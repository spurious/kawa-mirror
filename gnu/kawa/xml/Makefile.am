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

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
