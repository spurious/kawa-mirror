package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.ArrayType;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Hashtable;
import gnu.text.SourceMessages;
import gnu.kawa.util.*;
import gnu.kawa.lispexpr.*;
import gnu.kawa.reflect.ClassMemberConstraint;

public class Scheme extends Interpreter
{

  protected void define_proc (Named proc)
  {
    define (proc.getName (), proc);
  }

  protected void define_proc (String name, Named proc)
  {
    if (proc.getName() == null)
      proc.setName(name);
    define(name, proc);
  }

  /** Define a procedure to be autoloaded. */
  protected void define_proc (String name, String className)
  {
    define (name, new AutoloadProcedure (name, className, environ));
  }

  protected void define_syntax (String name, Syntax proc)
  {
    if (proc.getName() == null)
      proc.setName(name);
    define(name, proc);
  }

  protected void define_field (String name, String cname, String fname)
  {
    StaticFieldConstraint.define(environ, name, cname, fname);
  }

  protected void define_field (String name, String cname)
  {
    StaticFieldConstraint.define(environ, name, cname, Compilation.mangleName(name));
  }

  /* Define a Syntax to be autoloaded. */
  protected void define_syntax (String name, String className)
  {
    define (name, new AutoloadSyntax (name, className, environ));
  }

  public static Environment nullEnvironment;
  static Environment r4Environment;
  static Environment r5Environment;
  protected static Environment kawaEnvironment;

  public static SpecialType byteType = new SpecialType(Type.byte_type);
  public static SpecialType shortType = new SpecialType(Type.short_type);
  public static SpecialType intType = new SpecialType(Type.int_type);
  public static SpecialType longType = new SpecialType(Type.long_type);
  public static SpecialType floatType = new SpecialType(Type.float_type);
  public static SpecialType doubleType = new SpecialType(Type.double_type);
  public static SpecialType booleanType;
  public static SpecialType charType = new SpecialType(Type.char_type);
  public static SpecialType voidType = new SpecialType(Type.void_type);

  static Scheme instance;

  public static gnu.kawa.reflect.InstanceOf instanceOf;
  public static not not;
  public static kawa.standard.map map;
  public static kawa.standard.map forEach;
  public static gnu.kawa.functions.IsEq isEq;
  public static gnu.kawa.functions.IsEqv isEqv;
  public static gnu.kawa.functions.IsEqual isEqual;
  public static gnu.kawa.functions.Member memq;
  public static gnu.kawa.functions.Member memv;
  public static gnu.kawa.functions.Member member;
  public static gnu.kawa.functions.Assoc assq;
  public static gnu.kawa.functions.Assoc assv;
  public static gnu.kawa.functions.Assoc assoc;

  public static Scheme getInstance()
  {
    if (kawaEnvironment == null)
      new Scheme ();
    return instance;
  }

  public static synchronized Environment builtin ()
  {
    if (kawaEnvironment == null)
      new Scheme ();
    return kawaEnvironment;
  }

  public void initScheme ()
  {
      Named proc;
      Named syn;

      // (null-environment)
      nullEnvironment = new Environment ();
      nullEnvironment.setName ("null-environment");
      environ = nullEnvironment;

      Lambda lambda = new kawa.lang.Lambda();
      lambda.setKeywords(Special.optional, Special.rest, Special.key);
      define_syntax ("lambda", lambda);

      //-- Section 4.1  -- complete
      define (Interpreter.quote_sym, new Quote ());
      define_syntax("define", new kawa.standard.define(lambda, false));
      define_syntax("define-private",
                    new kawa.standard.define(lambda, true));
      define_syntax("define-constant",
                    new kawa.standard.define(lambda, false, true));
      define_syntax("define-autoload", new define_autoload(false));
      define_syntax("define-autoloads-from-file", new define_autoload(true));
      define_syntax ("if", "kawa.standard.ifp");
      define_syntax ("set!", "kawa.standard.set_b");

      // Section 4.2  -- complete
      define_syntax ("cond", "kawa.lib.std_syntax");
      define_syntax ("case", "kawa.lib.std_syntax");
      define_syntax ("and", "kawa.lib.std_syntax");
      define ("or", new kawa.standard.and_or (false, this));
      define_field("%let", "kawa.standard.let", "let");
      define_syntax ("let", "kawa.lib.std_syntax");
      define_syntax ("%let-decl", "kawa.lib.std_syntax");
      define_syntax ("%let-init", "kawa.lib.std_syntax");
      define_syntax ("let*", "kawa.lib.std_syntax");
      define_syntax ("letrec", "kawa.standard.letrec");

      define ("begin", new kawa.standard.begin());
      define_syntax ("do", "kawa.lib.std_syntax");
      define_syntax ("delay", "kawa.lib.std_syntax");
      define_proc ("%make-promise", "kawa.lib.std_syntax");
      define_syntax ("quasiquote", "kawa.standard.quasiquote");

      //-- Section 5  -- complete [except for internal definitions]

      // Appendix (and R5RS)
      define ("define-syntax", new kawa.standard.define_syntax ());
      define ("syntax-rules", new kawa.standard.syntax_rules ());
      define ("syntax-case", new kawa.standard.syntax_case ());
      define ("let-syntax", new kawa.standard.let_syntax (false));
      define ("letrec-syntax", new kawa.standard.let_syntax (true));

      r4Environment = new Environment (nullEnvironment);
      r4Environment.setName ("r4rs-environment");
      environ = r4Environment;

      //-- Section 6.1  -- complete
      define_field("not", "kawa.standard.Scheme");
      define_proc ("boolean?", "kawa.lib.misc");

      //-- Section 6.2  -- complete
      define_field("eq?", "kawa.standard.Scheme", "isEq");
      define_field("eqv?", "kawa.standard.Scheme", "isEqv");
      define_field("equal?", "kawa.standard.Scheme", "isEqual");

      //-- Section 6.3  -- complete
      define_proc("pair?", "kawa.lib.lists");
      define_proc("cons", "kawa.lib.lists");
      define_proc ("car", "kawa.lib.lists");
      define_proc ("cdr", "kawa.lib.lists");
      define_proc ("set-car!", "kawa.lib.lists");
      define_proc ("set-cdr!", "kawa.lib.lists");

      define_proc ("caar", "kawa.standard.cxr");
      define_proc ("cadr", "kawa.standard.cxr");
      define_proc ("cdar", "kawa.standard.cxr");
      define_proc ("cddr", "kawa.standard.cxr");
      define_proc ("caaar", "kawa.standard.cxr");
      define_proc ("caadr", "kawa.standard.cxr");
      define_proc ("cadar", "kawa.standard.cxr");
      define_proc ("caddr", "kawa.standard.cxr");
      define_proc ("cdaar", "kawa.standard.cxr");
      define_proc ("cdadr", "kawa.standard.cxr");
      define_proc ("cddar", "kawa.standard.cxr");
      define_proc ("cdddr", "kawa.standard.cxr");
      define_proc ("caaaar", "kawa.standard.cxr");
      define_proc ("caaadr", "kawa.standard.cxr");
      define_proc ("caadar", "kawa.standard.cxr");
      define_proc ("caaddr", "kawa.standard.cxr");
      define_proc ("cadaar", "kawa.standard.cxr");
      define_proc ("cadadr", "kawa.standard.cxr");
      define_proc ("caddar", "kawa.standard.cxr");
      define_proc ("cadddr", "kawa.standard.cxr");
      define_proc ("cdaaar", "kawa.standard.cxr");
      define_proc ("cdaadr", "kawa.standard.cxr");
      define_proc ("cdadar", "kawa.standard.cxr");
      define_proc ("cdaddr", "kawa.standard.cxr");
      define_proc ("cddaar", "kawa.standard.cxr");
      define_proc ("cddadr", "kawa.standard.cxr");
      define_proc ("cdddar", "kawa.standard.cxr");
      define_proc ("cddddr", "kawa.standard.cxr");
      define_proc ("null?", "kawa.lib.lists");
      define_field("list?", "kawa.lib.lists");
      define_field("list", "gnu.kawa.functions.MakeList");
      define_proc ("length", "kawa.lib.lists");
      define_proc ("append", "kawa.standard.append");
      define_proc ("reverse", "kawa.lib.lists");
      define_proc ("reverse!", "kawa.lib.lists");  // Not R5RS.
      define_field("list-tail", "kawa.lib.lists");
      define_field("list-ref", "kawa.lib.lists");

      define_field("memq", "kawa.standard.Scheme");
      define_field("memv", "kawa.standard.Scheme");
      define_field("member", "kawa.standard.Scheme");
      define_field("assq", "kawa.standard.Scheme");
      define_field("assv", "kawa.standard.Scheme");
      define_field("assoc", "kawa.standard.Scheme");

      //-- Section 6.4  -- complete, including slashified read/write
      
      define_proc ("symbol?", "kawa.lib.misc");
      define_proc ("symbol->string", "kawa.lib.misc");
      define_proc ("string->symbol", "kawa.lib.misc");

      //-- Section 6.5
      define_proc ("number?", "kawa.lib.numbers");
      define_proc ("quantity?", "kawa.lib.numbers");
      define_proc ("complex?", "kawa.lib.numbers");
      define_proc ("real?", "kawa.lib.numbers");
      define_proc ("rational?", "kawa.lib.numbers");
      define_proc ("integer?", "kawa.standard.integer_p");
      define_field("exact?", "kawa.lib.numbers");
      define_field("inexact?", "kawa.lib.numbers");
      define_proc ("=", NumberCompare.$Eq);
      define_proc ("<", NumberCompare.$Ls);
      define_proc (">", NumberCompare.$Gr);
      define_proc ("<=", NumberCompare.$Ls$Eq);
      define_proc (">=", NumberCompare.$Gr$Eq);
      define_proc ("zero?", "kawa.lib.numbers");
      define_proc ("positive?", "kawa.standard.positive_p");
      define_proc ("negative?", "kawa.lib.numbers");
      define_proc ("odd?", "kawa.lib.numbers");
      define_proc ("even?", "kawa.lib.numbers");
      define_proc ("max", "kawa.standard.max");
      define_proc ("min", "kawa.standard.min");
      define_proc ("+", gnu.kawa.functions.AddOp.$Pl);
      define_proc ("-", gnu.kawa.functions.AddOp.$Mn);
      define_proc ("*", "kawa.standard.multiply_oper");
      define_proc ("/", "kawa.standard.divide_oper");
      define_proc ("abs", "kawa.lib.numbers");
      define_proc ("quotient", "kawa.lib.numbers");
      define_proc ("remainder", "kawa.lib.numbers");
      define_proc ("modulo", "kawa.standard.modulo");
      define_proc ("gcd", "kawa.standard.gcd");
      define_proc ("lcm", "kawa.standard.lcm");
      define_proc ("numerator", "kawa.lib.numbers");
      define_proc ("denominator", "kawa.lib.numbers");
      define_proc ("floor", "kawa.lib.numbers");
      define_proc ("ceiling", "kawa.lib.numbers");
      define_proc ("truncate", "kawa.lib.numbers");
      define_proc ("round", "kawa.lib.numbers");
      define_proc ("rationalize", "kawa.standard.rationalize");
      define_proc ("exp", "kawa.lib.numbers");
      define_proc ("log", "kawa.lib.numbers");
      define_proc ("sin", "kawa.lib.numbers");
      define_proc ("cos", "kawa.lib.numbers");
      define_proc ("tan", "kawa.lib.numbers");
      define_proc ("asin", "kawa.lib.numbers");
      define_proc ("acos", "kawa.lib.numbers");
      define_proc ("atan", "kawa.standard.atan");
      define_proc ("sqrt", "kawa.standard.sqrt");
      define_proc ("expt", "kawa.standard.expt");
      define_proc ("make-rectangular", "kawa.lib.numbers");
      define_proc ("make-polar", "kawa.lib.numbers");
      define_proc ("real-part", "kawa.lib.numbers");
      define_proc ("imag-part", "kawa.lib.numbers");
      define_proc ("magnitude", "kawa.lib.numbers");
      define_proc ("angle", "kawa.lib.numbers");
      define_proc ("exact->inexact", "kawa.standard.exact2inexact");
      define_proc ("inexact->exact", "kawa.standard.inexact2exact");
      define_proc ("number->string", "kawa.lib.numbers");
      define_proc ("string->number", "kawa.standard.string2number");

      //-- Section 6.6  -- complete
      define_proc ("char?", "kawa.lib.characters");
      define_proc ("char=?", "kawa.lib.characters");
      define_proc ("char<?", "kawa.lib.characters");
      define_proc ("char>?", "kawa.lib.characters");
      define_proc ("char<=?", "kawa.lib.characters");
      define_proc ("char>=?", "kawa.lib.characters");
      define_proc ("char-ci=?", "kawa.lib.characters");
      define_proc ("char-ci<?", "kawa.lib.characters");
      define_proc ("char-ci>?", "kawa.lib.characters");
      define_proc ("char-ci<=?", "kawa.lib.characters");
      define_proc ("char-ci>=?", "kawa.lib.characters");
      define_proc ("char-alphabetic?", "kawa.lib.characters");
      define_proc ("char-numeric?", "kawa.lib.characters");
      define_proc ("char-whitespace?", "kawa.lib.characters");
      define_proc ("char-upper-case?", "kawa.lib.characters");
      define_proc ("char-lower-case?", "kawa.lib.characters");
      define_proc ("char->integer", "kawa.lib.characters");
      define_proc ("integer->char", "kawa.lib.characters");
      define_proc ("char-upcase", "kawa.lib.characters");
      define_proc ("char-downcase", "kawa.lib.characters");
      
      //-- Section 6.7  -- complete
      define_proc ("string?", "kawa.lib.strings");
      define_proc ("make-string", "kawa.lib.strings");
      define_proc ("string", "kawa.standard.string_v");
      define_proc ("string-length", "kawa.lib.strings");
      define_proc ("string-ref", "kawa.lib.strings");
      define_proc ("string-set!", "kawa.lib.strings");

      define_proc ("string=?", "kawa.lib.strings");
      define_proc ("string-ci=?", "kawa.standard.string_ci_equal_p");
      define_proc ("string<?", "kawa.standard.string_lessthan_p");
      define_proc ("string>?", "kawa.standard.string_greaterthan_p");
      define_proc ("string<=?", "kawa.standard.string_lessequal_p");
      define_proc ("string>=?", "kawa.standard.string_greaterequal_p");

      define_proc ("string-ci<?", "kawa.standard.string_ci_lessthan_p");
      define_proc ("string-ci>?", "kawa.standard.string_ci_greaterthan_p");
      define_proc ("string-ci<=?", "kawa.standard.string_ci_lessequal_p");
      define_proc ("string-ci>=?", "kawa.standard.string_ci_greaterequal_p");

      define_proc ("substring", "kawa.lib.strings");
      define_proc ("string-append", "kawa.standard.string_append");
      define_field("string->list", "kawa.lib.strings");
      define_field("list->string", "kawa.lib.strings");
      define_proc ("string-copy", "kawa.lib.strings");
      define_proc ("string-fill!", "kawa.lib.strings");

      //-- Section 6.8  -- complete
      define_proc ("vector?", "kawa.lib.vectors");
      define_proc ("make-vector", "kawa.lib.vectors");
      define_proc ("vector", "kawa.lib.vectors");
      define_proc ("vector-length", "kawa.lib.vectors");
      define_proc ("vector-ref", "kawa.lib.vectors");
      define_proc ("vector-set!", "kawa.lib.vectors");
      define_proc ("list->vector", "kawa.lib.vectors");
      define_field("vector->list", "kawa.lib.vectors");
      define_proc ("vector-fill!", "kawa.lib.vectors");
      // Extension:
      define_proc ("vector-append", "kawa.standard.vector_append");

      //-- Section 6.9  -- complete [except restricted call/cc]
      define_proc ("procedure?", "kawa.lib.misc");
      define_field("apply", "gnu.kawa.functions.Apply", "apply");
      define_field("map", "kawa.standard.Scheme", "map");
      define_field("for-each", "kawa.standard.Scheme", "forEach");
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");
      define_proc ("call/cc", "kawa.standard.callcc");
      define_proc ("force", "kawa.standard.force");

      //-- Section 6.10  -- complete
      define_proc ("call-with-input-file",
		   "kawa.standard.call_with_input_file");
      define_proc ("call-with-output-file",
		   "kawa.standard.call_with_output_file");
      define_proc ("input-port?", "kawa.lib.ports");
      define_proc ("output-port?", "kawa.lib.ports");
      define_proc ("current-input-port", "kawa.lib.ports");
      define_proc ("current-output-port", "kawa.lib.ports");
      define_proc ("with-input-from-file",
		   "kawa.standard.with_input_from_file");
      define_proc ("with-output-to-file",
		   "kawa.standard.with_output_to_file");
      define_proc ("open-input-file", "kawa.standard.open_input_file");
      define_proc ("open-output-file", "kawa.standard.open_output_file");
      define_proc ("close-input-port", "kawa.lib.ports");
      define_proc ("close-output-port", "kawa.lib.ports");
      define_proc ("read", "kawa.standard.read");
      define_proc ("read-line", "kawa.standard.read_line");
      define_proc (new readchar (false));  // read-char
      define_proc (new readchar (true));   // peek-char
      define_proc ("eof-object?", "kawa.lib.ports");
      define_proc ("char-ready?", "kawa.standard.char_ready_p");
      define_proc ("write", "kawa.lib.ports");
      define_field ("display", "kawa.lib.ports", "display");
      define_proc ("write-char", "kawa.lib.ports");
      define_proc ("newline", "kawa.lib.ports");
      define_proc ("load", "kawa.standard.load");
      define_proc ("transcript-off", "kawa.lib.ports");
      define_proc ("transcript-on", "kawa.lib.ports");
      define_proc ("call-with-input-string", "kawa.lib.ports");  // Extension
      define_proc ("open-input-string", "kawa.lib.ports");  // SRFI-6
      define_proc ("open-output-string", "kawa.lib.ports");  // SRFI-6
      define_proc ("get-output-string", "kawa.lib.ports");  // SRFI-6
      define_proc ("call-with-output-string",  // Extension
		   "kawa.standard.call_with_output_string");
      define_proc ("force-output", "kawa.lib.ports");  // Extension

      define_proc ("port-line", "kawa.lib.ports");
      define_proc ("set-port-line!", "kawa.lib.ports");
      define_proc ("port-column", "kawa.lib.ports");
      define_proc ("input-port-line-number", "kawa.lib.ports");  // Extension
      define_proc ("set-input-port-line-number!", "kawa.lib.ports");
      define_proc ("input-port-column-number", "kawa.lib.ports");
      define_proc ("input-port-read-state", "kawa.lib.ports");
      define_proc ("default-prompter", "kawa.lib.ports");
      define_proc ("input-port-prompter", "kawa.lib.ports");
      define_proc ("set-input-port-prompter!", "kawa.lib.ports");

      define_syntax ("%syntax-error", "kawa.standard.syntax_error");

      r5Environment = new Environment (r4Environment);
      r5Environment.setName ("r5rs-environment");
      environ = r5Environment;
      define_proc ("values", "kawa.lib.misc");
      define_proc ("call-with-values", "kawa.standard.call_with_values");
      define_proc ("eval", "kawa.lang.Eval");
      define_proc ("repl", new kawa.repl(this));
      define_proc ("scheme-report-environment", "kawa.standard.scheme_env");
      define_proc ("null-environment", "kawa.lib.misc");
      define_proc ("interaction-environment", "kawa.lib.misc");
      define_proc ("dynamic-wind", "kawa.lib.syntax");

      kawaEnvironment = new Environment (r5Environment);
      environ = kawaEnvironment;

      define_proc ("exit", "kawa.lib.thread");

      define_field("arithmetic-shift", "kawa.lib.numbers", "arithmeticShift");
      define_field("ash", "kawa.lib.numbers", "arithmeticShift");
      define_proc ("logand", "kawa.standard.logand");
      define_proc ("logior", "kawa.standard.logior");
      define_proc ("logxor", "kawa.standard.logxor");
      define_proc ("lognot", "kawa.lib.numbers");
      define_proc ("logop", "kawa.lib.numbers");
      define_proc ("logbit?", "kawa.lib.numbers");
      define_proc ("logtest", "kawa.lib.numbers");
      define_proc ("logcount", "kawa.lib.numbers");
      define_proc ("bit-extract", "kawa.lib.numbers");
      define_proc ("integer-length", "kawa.lib.numbers");

      // These are from SLIB.
      define_proc("string-upcase!", "kawa.lib.strings");
      define_proc("string-downcase!", "kawa.lib.strings");
      define_proc("string-capitalize!", "kawa.lib.strings");
      define_proc("string-upcase", "kawa.lib.strings");
      define_proc("string-downcase", "kawa.lib.strings");
      define_proc("string-capitalize", "kawa.lib.strings");
      define_syntax("primitive-virtual-method",
                    new kawa.standard.prim_method(182));
      define_syntax("primitive-static-method",
                    new kawa.standard.prim_method(184));
      define_syntax("primitive-interface-method",
                    new kawa.standard.prim_method(185));
      define_syntax("primitive-constructor",
                    new kawa.standard.prim_method(183));
      define_syntax("primitive-op1", new kawa.standard.prim_method());
      define_syntax("primitive-get-field", "kawa.lib.reflection");
      define_syntax("primitive-set-field", "kawa.lib.reflection");
      define_syntax("primitive-get-static", "kawa.lib.reflection");
      define_syntax("primitive-set-static", "kawa.lib.reflection");
      define_syntax("primitive-array-new", "kawa.lib.reflection");
      define_syntax("primitive-array-get", "kawa.lib.reflection");
      define_syntax("primitive-array-set", "kawa.lib.reflection");
      define_syntax("primitive-array-length", "kawa.lib.reflection");
      define_proc("subtype?", "kawa.lib.reflection");
      define_proc("primitive-throw", new kawa.standard.prim_throw());
      define_syntax("try-finally", "kawa.standard.try_finally");
      define_syntax("try-catch", "kawa.standard.try_catch");
      define_proc("throw", "kawa.standard.throw_name");
      define_proc("catch", "kawa.lib.syntax");
      define_proc("error", "kawa.lib.misc");
      define_proc("as", kawa.standard.convert.as);
      define_field("instance?", "kawa.standard.Scheme", "instanceOf");
      define_syntax("synchronized", "kawa.standard.synchronizd");
      object objectSyntax = new kawa.standard.object(lambda);
      define_syntax("object", objectSyntax);
      define_syntax("define-class",
                    new kawa.standard.define_class(objectSyntax));
      define_syntax("this", "kawa.lib.syntax");
      define_proc("make", gnu.kawa.reflect.Invoke.make);
      define_proc("slot-ref", gnu.kawa.reflect.SlotGet.field);
      define_proc("slot-set!", "gnu.kawa.reflect.SlotSet");
      define_proc("field", gnu.kawa.reflect.SlotGet.field);
      define_proc("class-methods", "gnu.kawa.reflect.ClassMethods");
      define_proc("static-field", gnu.kawa.reflect.SlotGet.staticField);
      define_proc("invoke", gnu.kawa.reflect.Invoke.invoke);
      define_proc("invoke-static", gnu.kawa.reflect.Invoke.invokeStatic);

      define_proc("file-exists?", "kawa.lib.files");
      define_proc("file-directory?", "kawa.lib.files");
      define_proc("file-readable?", "kawa.lib.files");
      define_proc("file-writable?", "kawa.lib.files");
      define_proc("delete-file", "kawa.lib.files");
      define_proc("system-tmpdir", "kawa.lib.files");
      define_proc("make-temporary-file", "kawa.lib.files");
      define_proc("rename-file", "kawa.lib.files");
      define_proc("copy-file", "kawa.lib.files");
      define_proc("create-directory", "kawa.lib.files");
      define_proc("->pathname", "kawa.lib.files");
      define("port-char-encoding", Boolean.TRUE);
      define("symbol-read-case", "P");

      define_proc("system", "kawa.lib.system");
      define_proc("make-process", "kawa.lib.system");
      define_proc("tokenize-string-to-string-array", "kawa.lib.system");
      define_proc("tokenize-string-using-shell", "kawa.lib.system");
      if ("/".equals(System.getProperty("file.separator")))
	define ("command-parse", lookup("tokenize-string-using-shell"));
      else
	define ("command-parse", lookup("tokenize-string-to-string-array"));
      
      // JDK 1.1 only:
      define_proc ("record-accessor", "kawa.lib.reflection");
      define_proc ("record-modifier", "kawa.lib.reflection");
      define_proc ("record-predicate", "kawa.lib.reflection");
      define_proc ("record-constructor", "kawa.lib.reflection");
      define_proc ("make-record-type", "kawa.lib.reflection");
      define_proc ("record-type-descriptor", "kawa.lib.reflection");
      define_proc ("record-type-name", "kawa.lib.reflection");
      define_proc ("record-type-field-names", "kawa.lib.reflection");
      define_proc ("record?", "kawa.lib.reflection");

      define_syntax ("when", "kawa.lib.syntax"); //-- (when cond exp ...)
      define_syntax ("unless", "kawa.lib.syntax"); //-- (unless cond exp ...)
      define_syntax ("fluid-let", "kawa.standard.fluid_let");
      define_syntax("constant-fold", "kawa.standard.constant_fold");

      define_proc ("compile-file", "kawa.lang.CompileFile");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
      define_proc ("environment-bound?", "kawa.lib.misc");
      define_proc ("scheme-implementation-version", "kawa.lib.misc");
      define_proc ("scheme-window", "kawa.lib.misc");
      define_syntax ("define-procedure", "kawa.lib.syntax");
      define_field("make-procedure",
                   "gnu.kawa.functions.MakeProcedure", "makeProcedure");
      define_proc ("procedure-property", "kawa.lib.misc");
      define_proc ("set-procedure-property!", "kawa.lib.misc");

      define_proc ("quantity->number", "kawa.standard.quantity2number");
      define_proc ("quantity->unit", "kawa.standard.quantity2unit");
      define_proc ("make-quantity", "kawa.standard.make_quantity");
      define_syntax ("define-unit", new kawa.standard.define_unit(false));
      define_syntax ("define-base-unit", new kawa.standard.define_unit(true));

      define_proc ("gentemp", "kawa.lib.syntax");
      define_syntax ("defmacro", "kawa.lib.syntax");
      define_field("setter", "gnu.kawa.functions.Setter", "setter");

      define_syntax ("future", "kawa.lib.thread");
      define_proc ("%make-future", "kawa.standard.make_future");
      define_proc ("sleep", "kawa.standard.sleep");

      define_syntax ("trace", "kawa.lib.trace");
      define_syntax ("untrace", "kawa.lib.trace");

      define_field("format", "gnu.kawa.functions.Format");
      define_field("parse-format", "gnu.kawa.functions.ParseFormat", "parseFormat");

      define_proc ("keyword?", "kawa.lib.keywords");
      define_proc ("keyword->string", "kawa.lib.keywords");
      define_proc ("string->keyword", "kawa.lib.keywords");
      define_syntax ("location", "kawa.standard.location");
      define ("define-alias", new kawa.standard.define_alias());
      define ("define-variable", new kawa.standard.define_variable());
      define ("define-member-alias", new kawa.standard.define_member_alias());
      define ("require", new kawa.standard.require());
      define_syntax ("module-name", "kawa.standard.module_name");
      define_syntax ("module-extends", "kawa.standard.module_extends");
      define_syntax ("module-implements", "kawa.standard.module_implements");
      define_syntax ("module-static", "kawa.standard.module_static");
      define_syntax ("module-export", "kawa.standard.export");

      define_proc ("s8vector?", "kawa.lib.uniform");
      define_proc ("make-s8vector", "kawa.lib.uniform");
      define_proc ("s8vector", "kawa.lib.uniform");
      define_proc ("s8vector-length", "kawa.lib.uniform");
      define_proc ("s8vector-ref", "kawa.lib.uniform");
      define_proc ("s8vector-set!", "kawa.lib.uniform");
      define_proc ("s8vector->list", "kawa.lib.uniform");
      define_proc ("list->s8vector", "kawa.lib.uniform");
      define_proc ("u8vector?", "kawa.lib.uniform");
      define_proc ("make-u8vector", "kawa.lib.uniform");
      define_proc ("u8vector", "kawa.lib.uniform");
      define_proc ("u8vector-length", "kawa.lib.uniform");
      define_proc ("u8vector-ref", "kawa.lib.uniform");
      define_proc ("u8vector-set!", "kawa.lib.uniform");
      define_proc ("u8vector->list", "kawa.lib.uniform");
      define_proc ("list->u8vector", "kawa.lib.uniform");

      define_proc ("s16vector?", "kawa.lib.uniform");
      define_proc ("make-s16vector", "kawa.lib.uniform");
      define_proc ("s16vector", "kawa.lib.uniform");
      define_proc ("s16vector-length", "kawa.lib.uniform");
      define_proc ("s16vector-ref", "kawa.lib.uniform");
      define_proc ("s16vector-set!", "kawa.lib.uniform");
      define_proc ("s16vector->list", "kawa.lib.uniform");
      define_proc ("list->s16vector", "kawa.lib.uniform");
      define_proc ("u16vector?", "kawa.lib.uniform");
      define_proc ("make-u16vector", "kawa.lib.uniform");
      define_proc ("u16vector", "kawa.lib.uniform");
      define_proc ("u16vector-length", "kawa.lib.uniform");
      define_proc ("u16vector-ref", "kawa.lib.uniform");
      define_proc ("u16vector-set!", "kawa.lib.uniform");
      define_proc ("u16vector->list", "kawa.lib.uniform");
      define_proc ("list->u16vector", "kawa.lib.uniform");

      define_proc ("s32vector?", "kawa.lib.uniform");
      define_proc ("make-s32vector", "kawa.lib.uniform");
      define_proc ("s32vector", "kawa.lib.uniform");
      define_proc ("s32vector-length", "kawa.lib.uniform");
      define_proc ("s32vector-ref", "kawa.lib.uniform");
      define_proc ("s32vector-set!", "kawa.lib.uniform");
      define_proc ("s32vector->list", "kawa.lib.uniform");
      define_proc ("list->s32vector", "kawa.lib.uniform");
      define_proc ("u32vector?", "kawa.lib.uniform");
      define_proc ("make-u32vector", "kawa.lib.uniform");
      define_proc ("u32vector", "kawa.lib.uniform");
      define_proc ("u32vector-length", "kawa.lib.uniform");
      define_proc ("u32vector-ref", "kawa.lib.uniform");
      define_proc ("u32vector-set!", "kawa.lib.uniform");
      define_proc ("u32vector->list", "kawa.lib.uniform");
      define_proc ("list->u32vector", "kawa.lib.uniform");

      define_proc ("s64vector?", "kawa.lib.uniform");
      define_proc ("make-s64vector", "kawa.lib.uniform");
      define_proc ("s64vector", "kawa.lib.uniform");
      define_proc ("s64vector-length", "kawa.lib.uniform");
      define_proc ("s64vector-ref", "kawa.lib.uniform");
      define_proc ("s64vector-set!", "kawa.lib.uniform");
      define_proc ("s64vector->list", "kawa.lib.uniform");
      define_proc ("list->s64vector", "kawa.lib.uniform");
      define_proc ("u64vector?", "kawa.lib.uniform");
      define_proc ("make-u64vector", "kawa.lib.uniform");
      define_proc ("u64vector", "kawa.lib.uniform");
      define_proc ("u64vector-length", "kawa.lib.uniform");
      define_proc ("u64vector-ref", "kawa.lib.uniform");
      define_proc ("u64vector-set!", "kawa.lib.uniform");
      define_proc ("u64vector->list", "kawa.lib.uniform");
      define_proc ("list->u64vector", "kawa.lib.uniform");

      define_proc ("f32vector?", "kawa.lib.uniform");
      define_proc ("make-f32vector", "kawa.lib.uniform");
      define_proc ("f32vector", "kawa.lib.uniform");
      define_proc ("f32vector-length", "kawa.lib.uniform");
      define_proc ("f32vector-ref", "kawa.lib.uniform");
      define_proc ("f32vector-set!", "kawa.lib.uniform");
      define_proc ("f32vector->list", "kawa.lib.uniform");
      define_proc ("list->f32vector", "kawa.lib.uniform");
      define_proc ("f64vector?", "kawa.lib.uniform");
      define_proc ("make-f64vector", "kawa.lib.uniform");
      define_proc ("f64vector", "kawa.lib.uniform");
      define_proc ("f64vector-length", "kawa.lib.uniform");
      define_proc ("f64vector-ref", "kawa.lib.uniform");
      define_proc ("f64vector-set!", "kawa.lib.uniform");
      define_proc ("f64vector->list", "kawa.lib.uniform");
      define_proc ("list->f64vector", "kawa.lib.uniform");

      define_proc ("emacs", "gnu.jemacs.buffer.emacs");
      define_proc ("node", "gnu.kawa.xml.MakeTreeNode");
  }

  static int scheme_counter = 0;

  public Scheme ()
  {
    if (Interpreter.defaultInterpreter == null)
      Interpreter.defaultInterpreter = this;
    if (kawaEnvironment == null)
      initScheme();
    environ = new ScmEnv (kawaEnvironment);
    environ.setName ("interaction-environment."+(++scheme_counter));
    if (instance == null)
      {
        instance = this;
        instanceOf = new gnu.kawa.reflect.InstanceOf(this, "instance?");
        not = new not(this, "not");
        map = new map(true);
	forEach = new map(false);
        isEq = new gnu.kawa.functions.IsEq(this, "eq?");
        isEqv = new gnu.kawa.functions.IsEqv(this, "eqv?");
        isEqual = new gnu.kawa.functions.IsEqual(this, "equal?");
        memq = new gnu.kawa.functions.Member("memq", isEq);
        memv = new gnu.kawa.functions.Member("memv", isEqv);
        member = new gnu.kawa.functions.Member("member", isEqual);
        assq = new gnu.kawa.functions.Assoc("assq", isEq);
        assv = new gnu.kawa.functions.Assoc("assv", isEqv);
        assoc = new gnu.kawa.functions.Assoc("assoc", isEqual);
      }
  }

  public Scheme (Environment environ)
  {
    this.environ = environ;
  }

  public String getName()
  {
    return "Scheme";
  }

  public Environment getNewEnvironment ()
  {
    if (kawaEnvironment == null)
      initScheme();
    Environment environ = new ScmEnv (kawaEnvironment);
    environ.setName ("interaction-environment."+(++scheme_counter));
    return environ;
  }

  /** Evalutate Scheme expressions from string.
   * @param string the string constaining Scheme expressions
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Interpreter.voidObject if none. */
  public static Object eval (String string, Environment env)
  {
    return eval (new CharArrayInPort(string), env);
  }

  public Object eval (String string)
  {
    return eval(string, environ);
  }

  /** Evalutate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Interpreter.voidObject if none. */
  public static Object eval (InPort port, Environment env)
  {
    try
      {
	SourceMessages messages = new SourceMessages();
        Object body = CompileFile.readBody(port, messages);
        if (messages.seenErrors())
          throw new gnu.text.SyntaxException(messages);
	return Eval.evalBody(body, env, messages);
      }
    catch (gnu.text.SyntaxException e)
      {
	// The '\n' is because a SyntaxException includes a line number,
	// and it is better if that starts the line.  FIXME OBSOLETE
	throw new RuntimeException("eval: errors while compiling:\n"
				   +e.getMessages().toString(20));
      }
  }

  /** Evalutate Scheme expressions from an "S expression."
   * @param sexpr the S expression to evaluate
   * @param env the Environment to evaluate the string in
   * @return result of the expression. */
  public static Object eval (Object sexpr, Environment env)
  {
    return Eval.eval (sexpr, env);
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return ScmRead.readObject(in);
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new ScmRead(inp, messages);
  }

  public void print (Object value, OutPort out)
  {
    if (value == Scheme.voidObject)
      return;
    if (value instanceof Values)
      {
	Object[] values = ((Values) value).getValues();
	for (int i = 0;  i < values.length;  i++)
	  {
	    SFormat.print (values[i], out);
	    out.println();
	  }
      }
    else
      {
	SFormat.print (value, out);
	out.println();
      }
    out.flush();
  }

  /** If exp is a "constant" Type, return that type, otherwise return null. */
  public static Type getTypeValue (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  public static ModuleExp makeModuleExp(Object body, Translator tr)
  {
    ModuleExp mexp = new ModuleExp();
    java.util.Vector forms = new java.util.Vector(20);
    SourceMessages messages = tr.getMessages();
    tr.push(mexp);
    tr.scan_body(body, forms, mexp);
    tr.finishModule(mexp, forms);
    return mexp;
  }

  static Hashtable types;

  public static Type getNamedType (String name)
  {
    if (types == null)
      {
	booleanType = new SpecialType(Type.boolean_type,
				      Scheme.getInstance());
	types = new Hashtable ();
	types.put ("void", Scheme.voidType);
	types.put ("int", Scheme.intType);
	types.put ("char", Scheme.charType);
	types.put ("boolean", Scheme.booleanType);
	types.put ("byte", Scheme.byteType);
	types.put ("short", Scheme.shortType);
	types.put ("long", Scheme.longType);
	types.put ("float", Scheme.floatType);
	types.put ("double", Scheme.doubleType);

	types.put ("Object", Type.pointer_type);
	types.put ("java.lang.Object", Type.pointer_type);
	types.put ("String", Type.string_type);

	types.put ("object", Type.pointer_type);
	types.put ("number", ClassType.make("gnu.math.Numeric"));
	types.put ("quantity", ClassType.make("gnu.math.Quantity"));
	types.put ("complex", ClassType.make("gnu.math.Complex"));
	types.put ("real", ClassType.make("gnu.math.RealNum"));
	types.put ("rational", ClassType.make("gnu.math.RatNum"));
	types.put ("integer", ClassType.make("gnu.math.IntNum"));
	types.put ("symbol", ClassType.make("java.lang.String"));
	types.put ("keyword", ClassType.make("gnu.expr.Keyword"));
	types.put ("list", ClassType.make("gnu.lists.LList"));
	types.put ("pair", ClassType.make("gnu.lists.Pair"));
	types.put ("pair-with-position",
		   ClassType.make("gnu.lists.PairWithPosition"));
	types.put ("string", ClassType.make("gnu.lists.FString"));
	types.put ("abstract-string", ClassType.make("gnu.lists.CharSequence"));
	types.put ("character", ClassType.make("gnu.text.Char"));
	types.put ("vector", ClassType.make("gnu.lists.FVector"));
	types.put ("function", ClassType.make("gnu.mapping.Procedure"));
	types.put ("procedure", ClassType.make("gnu.mapping.Procedure"));
	types.put ("input-port", ClassType.make("gnu.mapping.InPort"));
	types.put ("output-port", ClassType.make("gnu.mapping.OutPort"));
	types.put ("string-output-port",
                   ClassType.make("gnu.mapping.CharArrayOutPort"));
	types.put ("record", ClassType.make("kawa.lang.Record"));
	types.put ("type", ClassType.make("gnu.bytecode.Type"));
	types.put ("class-type", ClassType.make("gnu.bytecode.ClassType"));

        types.put ("s8vector", ClassType.make("gnu.lists.S8Vector"));
        types.put ("u8vector", ClassType.make("gnu.lists.U8Vector"));
        types.put ("s16vector", ClassType.make("gnu.lists.S16Vector"));
        types.put ("u16vector", ClassType.make("gnu.lists.U16Vector"));
        types.put ("s32vector", ClassType.make("gnu.lists.S32Vector"));
        types.put ("u32vector", ClassType.make("gnu.lists.U32Vector"));
        types.put ("s64vector", ClassType.make("gnu.lists.S64Vector"));
        types.put ("u64vector", ClassType.make("gnu.lists.U64Vector"));
        types.put ("f32vector", ClassType.make("gnu.lists.F32Vector"));
        types.put ("f64vector", ClassType.make("gnu.lists.F64Vector"));
        types.put ("document", ClassType.make("gnu.lists.TreeList"));
      }
    Type type = (Type) types.get(name);
    if (type == null && name.startsWith("elisp:"))
      {
	Class clas = getNamedType(name.substring(6)).getReflectClass();
	type = Interpreter.getInstance("elisp").getTypeFor(clas);
	types.put(name, type);
      }
    return type;
  }

  public Type getTypeFor (Class clas)
  {
    if (clas.isPrimitive())
      return getNamedType(clas.getName());
    return Type.make(clas);
  }

  public static Type string2Type (String name)
  {
    Type t;
    if (name.endsWith("[]"))
      {
	t = string2Type(name.substring(0, name.length()-2));
	if (t != null)
	  t = gnu.bytecode.ArrayType.make(t);
      }
    else
      t = getNamedType (name);
    if (t != null)
      return t;
    t = Interpreter.string2Type(name);
    if (t != null)
      types.put (name, t);
    return t;
  }

  public Type getTypeFor(String name)
  {
    return string2Type(name);
  }

  /** Convert expression to a Type.
   * Allow "TYPE" or 'TYPE or <TYPE>.
   */
  public static Type exp2Type (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Scheme interp = new Scheme();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public static String demangleName(String name)
  {
    StringBuffer sbuf = new StringBuffer();
    int len = name.length();
    boolean mangled = false;
    boolean predicate = false;
    boolean downCaseNext = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt(i);
	if (downCaseNext)
	  {
	    ch = Character.toLowerCase(ch);
	    downCaseNext = false;
	  }
	char d;
	if (ch == 'i' && i == 0 && len > 2 && name.charAt(i+1) == 's'
	    && ! Character.isLowerCase(d = name.charAt(i+2)))
	  {
	    mangled = true;
	    predicate = true;
	    i++;
	    if (Character.isUpperCase(d) || Character.isTitleCase(d))
	      {
		sbuf.append(Character.toLowerCase(d));
		i++;
		continue;
	      }
	    continue;
	  }
	else if (ch == '$' && i + 2 < len)
	  {
	    char c1 = name.charAt(i+1);
	    char c2 = name.charAt(i+2);
	    d = Compilation.demangle2(c1, c2);
	    if (d != (char)(-1))
	      {
		sbuf.append(d);
		i += 2;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	    else if (c1 == 'T' && c2 == 'o' && i + 3 < len
		     && name.charAt(i+3) == '$')
	      {
		sbuf.append("->");
		i += 3;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	  }
	else if (i > 1
		 && (Character.isUpperCase(ch) || Character.isTitleCase(ch))
		 && (Character.isLowerCase(name.charAt(i-1))))
	  {
	    sbuf.append('-');
	    mangled = true;
	    ch = Character.toLowerCase(ch);
	  }
	sbuf.append(ch);
      }
    if (predicate)
      sbuf.append('?');
    return mangled ? sbuf.toString() : name;
  }

}
