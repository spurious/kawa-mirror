## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = codegen kawa doc testsuite
javadir=$(prefix)/java

# Build a .zip archive from the compiled classes.
# Assumes you have done a 'make install' first.

kawa-@VERSION@-compiled.zip:
	(cd $(javadir); zip -r -n .class - kawa codegen) > $@
