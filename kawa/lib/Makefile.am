## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

KAWA = $(JAVA) kawa.repl
JAVAC = $(KAWA)
JAVACFLAGS = -P kawa.lib. -C

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
  quantities.scm \
  strings.scm \
  syntax.scm \
  vectors.scm \
  system.scm \
  emacs.scm

SOURCES = $(java_JAVA)
