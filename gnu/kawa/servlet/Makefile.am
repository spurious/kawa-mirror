## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  CGIServletWrapper.java \
  GetRequest.java \
  GetResponse.java \
  KawaPageServlet.java \
  KawaServlet.java \
  ReplServlet.java \
  ReplSession.java \
  ServletCallContext.java \
  ServletPrinter.java

java_SCM = HTTP.scm

PACKAGE_FNAME = gnu-kawa-servlet
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
EXTRA_GCJ_INPUTS =  *.class
EXTRA_GCJ_COMPILED = $(OFILES_DIR)/kawa1$(exeext)

if ENABLE_SERVLET
ALL = java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)
