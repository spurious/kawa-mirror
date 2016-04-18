## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=

java_SCM = defs.scm utils.scm ViewBuilder.scm 

PACKAGE_FNAME = gnu-kawa-android
SCM_COMPILE_FLAGS = --warn-undefined-variable --warn-unknown-member --warn-as-error
EXTRA_DIST = custom_rules.xml

all: Makefile scm-classes.stamp
