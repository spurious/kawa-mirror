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
  keywords.class \
  ports.class \
  quantities.class \
  strings.class \
  syntax.class \
  vectors.class \
  @EXTRA_OBJ@

OBJ_1_0 = 
OBJ_1_1 = reflection.class

EXTRA_DATA = $(OBJ_1_0) $(OBJ_1_1)

# These are generated automatically when the $(OBJ) files are built,
# but they also need to be installed.
# The funny names are because the Scheme functions names get "mangled"
# into valid Java class names.
SECONDARY_OBJS=\
  _0025defmacro.class\
  char_002d_003einteger.class\
  char_002ddowncase.class\
  char_002dlower_002dcase_003f.class\
  char_002dnumeric_003f.class\
  char_002dupcase.class\
  char_002dupper_002dcase_003f.class\
  char_002dwhitespace_003f.class\
  eof_002dobject_003f.class\
  exit.class\
  force_002doutput.class\
  gentemp.class\
  integer_002d_003echar.class\
  keyword_002d_003estring.class\
  keyword_003f.class\
  keywords.class\
  list_002d_003evector.class\
  make_002dstring.class\
  newline.class\
  string_002d_003ekeyword.class\
  string_002dlength.class\
  vector_002dlength.class\
  vector_002dref.class\
  vector_002dset_0021.class\
  @EXTRA_MISC@

MISC_1_1 = \
  lambda.class\
  make_002drecord_002dtype.class\
  record_002daccessor.class\
  record_002dconstructor.class\
  record_002dmodifier.class\
  record_002dpredicate.class\
  record_002dtype_002ddescriptor.class\
  record_002dtype_002dfield_002dnames.class\
  record_002dtype_002dname.class\
  record_003f.class

java_DATA=$(OBJ) $(SECONDARY_OBJS)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.scm)

CLEANFILES = *.class

all: $(OBJ)
