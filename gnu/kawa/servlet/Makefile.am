## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  CGIServletWrapper.java \
  GetRequest.java \
  GetResponse.java \
  KawaPageServlet.java \
  KawaServlet.java \
  ServletCallContext.java \
  ServletPrinter.java

PACKAGE_FNAME = gnu-kawa-servlet

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
