## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  AncestorAxis.java \
  AncestorOrSelfAxis.java \
  AttributeAxis.java \
  Attributes.java \
  AttributeConstructor.java \
  AttributeType.java \
  ChildAxis.java \
  CoerceNodes.java \
  DefineNamespace.java \
  DescendantAxis.java \
  DescendantOrSelfAxis.java \
  Document.java \
  DocumentConstructor.java \
  ElementConstructor.java \
  ElementType.java \
  Focus.java \
  FollowingAxis.java \
  FollowingSiblingAxis.java \
  HttpPrinter.java \
  IntersectNodes.java \
  IteratorItems.java \
  KAttr.java \
  KDocument.java \
  KElement.java \
  KNode.java \
  KText.java \
  ListItems.java \
  MakeResponseHeader.java \
  MakeUnescapedData.java \
  NamespaceEnv.java \
  NodeCompare.java \
  NodeConstructor.java \
  NodeName.java \
  Nodes.java \
  NodeType.java \
  OutputAsXML.java \
  ParentAxis.java \
  PrecedingAxis.java \
  PrecedingSiblingAxis.java \
  SelfAxis.java \
  SortNodes.java \
  SortedNodes.java \
  TextConstructor.java \
  TreeScanner.java \
  UnionNodes.java \
  WriteTo.java \
  XMLParser.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
