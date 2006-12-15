## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_FUTURE = \
  ParsedXMLPrinter.java \
  MakeTreeNode.java \
  TreeNode.java \
  SimpleFilter.java

java_sources = \
  NamedChildrenFilter.java \
  NamespaceBinding.java \
  NodeTree.java \
  TextUtils.java \
  XMLFilter.java \
  XMLParser.java \
  XMLPrinter.java \
  XName.java

BUILT_SOURCES = $(java_BUILT)
EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-xml
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
