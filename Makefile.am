## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = gnu kawa doc testsuite
javadir=$(datadir)/java

# Build a .zip archive from the compiled classes.

kawa-@VERSION@-compiled.zip:
	zip -q -r -n .class $@ kawa gnu -i '*.class'

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs
