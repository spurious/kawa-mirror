## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

KAWA = $(JAVA) kawa.repl
JAVAC = $(KAWA)
JAVACFLAGS = -P kawa.lib. --module-static -C

javadir=$(datadir)/java/$(subdir)

java_JAVA = $(java_SCM)

java_SCM=\
  std_syntax.scm \
  reflection.scm \
  lists.scm \
  thread.scm \
  characters.scm \
  files.scm \
  keywords.scm \
  ports.scm \
  misc.scm \
  numbers.scm \
  strings.scm \
  syntax.scm \
  vectors.scm \
  uniform.scm \
  system.scm \
  trace.scm \
  emacs.scm

SOURCES = $(java_JAVA)

# Used to generate makekawa.bat in ../.. .
list-sources:
	echo $(SOURCES)
