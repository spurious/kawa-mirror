# Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

JAVAX_SCRIPT_EXTRA=\
  SchemeScriptEngineFactory.java
if WITH_JAVAX_SCRIPT
  JAVAX_SCRIPT_USED = $(JAVAX_SCRIPT_EXTRA)
endif

java_sources = \
set_b.java \
prim_method.java prim_throw.java throw_name.java \
append.java \
fluid_let.java \
expt.java \
vector_append.java \
readchar.java \
read_line.java \
char_ready_p.java \
load.java \
constant_fold.java \
define.java \
define_syntax.java syntax_rules.java let_syntax.java \
syntax.java syntax_case.java \
require.java module_extends.java module_implements.java module_name.java \
module_compile_options.java with_compile_options.java \
module_static.java export.java define_unit.java \
syntax_error.java \
let.java \
begin.java \
call_with_values.java \
sleep.java \
try_catch.java \
make.java location.java \
define_alias.java define_variable.java define_autoload.java \
define_member_alias.java \
Scheme.java SchemeCompilation.java \
object.java define_class.java thisRef.java \
  IfFeature.java \
  ImportFromLibrary.java \
  $(JAVAX_SCRIPT_USED) \
  TracedProcedure.java

PACKAGE_FNAME = kawa-standard
GCJ_INPUTS =  $(java_Java)
EXTRA_DIST = $(JAVAX_SCRIPT_EXTRA)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
