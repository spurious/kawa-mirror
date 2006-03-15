## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  AncestorAxis.java \
  AncestorOrSelfAxis.java \
  AttributeAxis.java \
  Attributes.java \
  AttributeType.java \
  ChildAxis.java \
  Children.java \
  CoerceNodes.java \
  CommentConstructor.java \
  DescendantAxis.java \
  DescendantOrSelfAxis.java \
  Document.java \
  DocumentConstructor.java \
  ElementType.java \
  Focus.java \
  FollowingAxis.java \
  FollowingSiblingAxis.java \
  HttpPrinter.java \
  IntersectNodes.java \
  IteratorItems.java \
  KAttr.java \
  KCDATASection.java \
  KCharacterData.java \
  KComment.java \
  KDocument.java \
  KElement.java \
  KNode.java \
  KProcessingInstruction.java \
  KText.java \
  ListItems.java \
  MakeAttribute.java \
  MakeCDATA.java \
  MakeElement.java \
  MakeProcInst.java \
  MakeResponseHeader.java \
  MakeText.java \
  MakeUnescapedData.java \
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
  StringValue.java \
  TreeScanner.java \
  UnionNodes.java \
  UntypedAtomic.java \
  WriteTo.java \
  XDataType.java \
  XInteger.java \
  XIntegerType.java \
  XTimeType.java \
  XMLParser.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
