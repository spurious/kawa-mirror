## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVA = java
JAVAFLAGS = -classpath ../..:$$CLASSPATH

JAVAC = javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH
KAWA = $(JAVA) $(JAVAFLAGS) kawa.repl

.java.class:
	$(JAVAC) -d ../.. $<

.scm.class:
	$(KAWA) -d ../.. -P $(subdir). -C $< 

javadir=$(prefix)/java/$(subdir)

OBJ=\
  std_syntax.class \
  thread.class \
  characters.class \
  vectors.class \
  when_unless.class

# These are generated automatically when the $(OBJ) files are built,
# but they also need to be installed.
# The funny names are because the Scheme functions names get "mangled"
# into valid Java class names.
SECONDARY_OBJS=\
  char_002d_003einteger.class\
  char_002ddowncase.class\
  char_002dlower_002dcase_003f.class\
  char_002dnumeric_003f.class\
  char_002dupcase.class\
  char_002dupper_002dcase_003f.class\
  char_002dwhitespace_003f.class\
  integer_002d_003echar.class\
  list_002d_003evector.class\
  vector_002dlength.class\
  vector_002dref.class\
  vector_002dset_0021.class

java_DATA=$(OBJ) $(SECONDARY_OBJS)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.scm)

CLEANFILES = *.class

all: $(OBJ)
