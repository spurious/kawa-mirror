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
  characters$$char_2_integer.class \
  characters$$char__downcase.class \
  characters$$char__lower__case_P.class \
  characters$$char__numeric_P.class \
  characters$$char__upcase.class \
  characters$$char__upper__case_P.class \
  characters$$char__whitespace_P.class \
  characters$$integer_2_char.class \
  keywords$$keyword_2_string.class \
  keywords$$keyword_P.class \
  keywords$$string_2_keyword.class \
  ports$$eof__object_P.class \
  ports$$force__output.class \
  ports$$newline.class \
  strings$$make__string.class \
  strings$$string__length.class \
  syntax$$_Cdefmacro.class \
  syntax$$gentemp.class \
  thread$$exit.class \
  vectors$$list_2_vector.class \
  vectors$$vector__length.class \
  vectors$$vector__ref.class \
  vectors$$vector__set_B.class \
  @EXTRA_MISC@

MISC_1_1 = \
  reflection$$lambda.class \
  reflection$$make__record__type.class \
  reflection$$record_P.class \
  reflection$$record__accessor.class \
  reflection$$record__constructor.class \
  reflection$$record__modifier.class \
  reflection$$record__predicate.class \
  reflection$$record__type__descriptor.class \
  reflection$$record__type__field__names.class \
  reflection$$record__type__name.class

java_DATA=$(OBJ) $(SECONDARY_OBJS)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.scm)

CLEANFILES = *.class

all: $(OBJ)
