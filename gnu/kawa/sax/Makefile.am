## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  ContentConsumer.java \
  KawaXMLReader.java

PACKAGE_FNAME = gnu-kawa-sax

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
