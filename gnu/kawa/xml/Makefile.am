## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Attributes.java \
  AttributeConstructor.java \
  DefineNamespace.java \
  Document.java \
  DocumentConstructor.java \
  ElementConstructor.java \
  ElementType.java \
  Focus.java \
  HttpPrinter.java \
  IteratorItems.java \
  ListItems.java \
  MakeResponseHeader.java \
  MakeUnescapedData.java \
  NamedAttributes.java \
  NamedChildren.java \
  NamedDescendants.java \
  NamespaceEnv.java \
  NodeCompare.java \
  NodeName.java \
  NodeType.java \
  OutputAsXML.java \
  TextConstructor.java \
  WriteTo.java \
  XMLParser.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
