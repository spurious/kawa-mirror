## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  AncestorAxis.java \
  AncestorOrSelfAxis.java \
  AttributeAxis.java \
  Attributes.java \
  AttributeType.java \
  Base64Binary.java \
  BinaryObject.java \
  ChildAxis.java \
  Children.java \
  CoerceNodes.java \
  CommentConstructor.java \
  CompileXmlFunctions.java \
  DescendantAxis.java \
  DescendantOrSelfAxis.java \
  Document.java \
  DocumentConstructor.java \
  ElementType.java \
  Focus.java \
  FollowingAxis.java \
  FollowingSiblingAxis.java \
  HexBinary.java \
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
  MakeWithBaseUri.java \
  NodeCompare.java \
  NodeConstructor.java \
  Nodes.java \
  NodeSetType.java \
  NodeType.java \
  Notation.java \
  OutputAsXML.java \
  ParentAxis.java \
  PrecedingAxis.java \
  PrecedingSiblingAxis.java \
  ProcessingInstructionType.java \
  SelfAxis.java \
  SortNodes.java \
  SortedNodes.java \
  TreeScanner.java \
  UnionNodes.java \
  UntypedAtomic.java \
  WriteTo.java \
  XDataType.java \
  XInteger.java \
  XIntegerType.java \
  XString.java \
  XStringType.java \
  XTimeType.java \
  XmlNamespace.java

PACKAGE_FNAME = gnu-kawa-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
