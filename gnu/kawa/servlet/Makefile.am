## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_sources=\
  CGIServletWrapper.java \
  GetRequest.java \
  GetResponse.java \
  KawaServlet.java \
  ServletCallContext.java \
  ServletPrinter.java

PACKAGE_FNAME = gnu-kawa-servlet
SCM_COMPILE_FLAGS = --full-tailcalls
GCJ_INPUTS =  $(java_Java)
SOURCES = $(java_SCM) $(java_sources)
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
java_SCM = HTTP.scm
EXTRA_DIST = $(java_SCM)

./../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)

all: Makefile java-classes.stamp ../../../kawa/java-classes.stamp \
  scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
