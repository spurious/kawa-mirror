package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.ArrayType;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Hashtable;
import gnu.text.SourceMessages;
import gnu.kawa.lispexpr.*;
import gnu.lists.FormatToConsumer;
import gnu.kawa.functions.DisplayFormat;

public class Scheme extends LispInterpreter
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

  /* Define a Syntax to be autoloaded. */
  protected void define_syntax (String name, String className)
  {
    define (name, new AutoloadSyntax (name, className, environ));
  }

  public static Environment nullEnvironment;
  public static Environment r4Environment;
  public static Environment r5Environment;
  protected static Environment kawaEnvironment;

  public static LangPrimType booleanType;
  static Scheme instance;

  public static gnu.kawa.reflect.InstanceOf instanceOf;
  public static not not;
  public static kawa.standard.map map;
  public static kawa.standard.map forEach;
  public static gnu.kawa.functions.IsEq isEq;
  public static gnu.kawa.functions.IsEqv isEqv;
  public static gnu.kawa.functions.IsEqual isEqual;

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
      define (LispInterpreter.quote_sym, new Quote ());
      define_syntax("%define", new kawa.standard.define(lambda));
      define_syntax ("define", "kawa.lib.prim_syntax");

      define_syntax ("if", "kawa.lib.prim_syntax");
      define_syntax ("set!", "kawa.standard.set_b");

      // Section 4.2  -- complete
      define_syntax ("cond", "kawa.lib.std_syntax");
      define_syntax ("case", "kawa.lib.std_syntax");
      define_syntax ("and", "kawa.lib.std_syntax");
      define_syntax ("or", "kawa.lib.std_syntax");
      define_field("%let", "kawa.standard.let", "let");
      define_syntax ("let", "kawa.lib.std_syntax");
      define_syntax ("%let-decl", "kawa.lib.std_syntax");
      define_syntax ("%let-init", "kawa.lib.std_syntax");
      define_syntax ("let*", "kawa.lib.std_syntax");
      define_syntax ("letrec", "kawa.lib.std_syntax");

      define ("begin", new kawa.standard.begin());
      define_syntax ("do", "kawa.lib.std_syntax");
      define_syntax ("delay", "kawa.lib.std_syntax");
      define_proc ("%make-promise", "kawa.lib.std_syntax");
      define_syntax ("quasiquote", "kawa.standard.quasiquote");

      //-- Section 5  -- complete [except for internal definitions]

      // Appendix (and R5RS)
      define_syntax ("define-syntax", "kawa.lib.prim_syntax");
      define ("%define-syntax", new kawa.standard.define_syntax ());
      define ("syntax-rules", new kawa.standard.syntax_rules ());
      define ("syntax", new kawa.standard.syntax ());
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
      define_field("cons", "kawa.lib.lists");
      define_field("car", "kawa.lib.lists");
      define_field("cdr", "kawa.lib.lists");
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
      define_field("append", "kawa.standard.append", "append");
      define_proc ("reverse", "kawa.lib.lists");
      define_proc ("reverse!", "kawa.lib.lists");  // Not R5RS.
      define_field("list-tail", "kawa.lib.lists");
      define_field("list-ref", "kawa.lib.lists");

      define_field("memq", "kawa.lib.lists");
      define_field("memv", "kawa.lib.lists");
      define_field("member", "kawa.lib.lists");
      define_field("assq", "kawa.lib.lists");
      define_field("assv", "kawa.lib.lists");
      define_field("assoc", "kawa.lib.lists");

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
      define_field("=", "gnu.kawa.functions.NumberCompare", "$Eq");
      define_field("<", "gnu.kawa.functions.NumberCompare", "$Ls");
      define_field(">", "gnu.kawa.functions.NumberCompare", "$Gr");
      define_field("<=", "gnu.kawa.functions.NumberCompare", "$Ls$Eq");
      define_field(">=", "gnu.kawa.functions.NumberCompare", "$Gr$Eq");
      define_proc ("zero?", "kawa.lib.numbers");
      define_proc ("positive?", "kawa.lib.numbers");
      define_proc ("negative?", "kawa.lib.numbers");
      define_proc ("odd?", "kawa.lib.numbers");
      define_proc ("even?", "kawa.lib.numbers");
      define_proc ("max", "kawa.standard.max");
      define_proc ("min", "kawa.standard.min");
      define_field("+", "gnu.kawa.functions.AddOp", "$Pl");
      define_field("-", "gnu.kawa.functions.AddOp", "$Mn");
      define_field("*", "gnu.kawa.functions.MultiplyOp", "$St");
      define_field("/", "gnu.kawa.functions.DivideOp", "$Sl");
      define_proc ("abs", "kawa.lib.numbers");
      define_proc ("quotient", "kawa.lib.numbers");
      define_proc ("remainder", "kawa.lib.numbers");
      define_proc ("modulo", "kawa.lib.numbers");
      define_proc ("gcd", "kawa.standard.gcd");
      define_proc ("lcm", "kawa.standard.lcm");
      define_proc ("numerator", "kawa.lib.numbers");
      define_proc ("denominator", "kawa.lib.numbers");
      define_proc ("floor", "kawa.lib.numbers");
      define_proc ("ceiling", "kawa.lib.numbers");
      define_proc ("truncate", "kawa.lib.numbers");
      define_proc ("round", "kawa.lib.numbers");
      define_proc ("rationalize", "kawa.lib.numbers");
      define_proc ("exp", "kawa.lib.numbers");
      define_proc ("log", "kawa.lib.numbers");
      define_proc ("sin", "kawa.lib.numbers");
      define_proc ("cos", "kawa.lib.numbers");
      define_proc ("tan", "kawa.lib.numbers");
      define_proc ("asin", "kawa.lib.numbers");
      define_proc ("acos", "kawa.lib.numbers");
      define_proc ("atan", "kawa.standard.atan");
      define_proc ("sqrt", "kawa.lib.numbers");
      define_field ("expt", "kawa.standard.expt");
      define_proc ("make-rectangular", "kawa.lib.numbers");
      define_proc ("make-polar", "kawa.lib.numbers");
      define_proc ("real-part", "kawa.lib.numbers");
      define_proc ("imag-part", "kawa.lib.numbers");
      define_proc ("magnitude", "kawa.lib.numbers");
      define_proc ("angle", "kawa.lib.numbers");
      define_proc ("exact->inexact", "kawa.lib.numbers");
      define_proc ("inexact->exact", "kawa.lib.numbers");
      define_proc ("number->string", "kawa.lib.numbers");
      define_proc ("string->number", "kawa.standard.string2number");

      //-- Section 6.6  -- complete
      defProcStFld("char?", "kawa.lib.characters");
      defProcStFld("char=?", "kawa.lib.characters");
      defProcStFld("char<?", "kawa.lib.characters");
      defProcStFld("char>?", "kawa.lib.characters");
      defProcStFld("char<=?", "kawa.lib.characters");
      defProcStFld("char>=?", "kawa.lib.characters");
      defProcStFld("char-ci=?", "kawa.lib.characters");
      defProcStFld("char-ci<?", "kawa.lib.characters");
      defProcStFld("char-ci>?", "kawa.lib.characters");
      defProcStFld("char-ci<=?", "kawa.lib.characters");
      defProcStFld("char-ci>=?", "kawa.lib.characters");
      defProcStFld("char-alphabetic?", "kawa.lib.characters");
      defProcStFld("char-numeric?", "kawa.lib.characters");
      defProcStFld("char-whitespace?", "kawa.lib.characters");
      defProcStFld("char-upper-case?", "kawa.lib.characters");
      defProcStFld("char-lower-case?", "kawa.lib.characters");
      defProcStFld("char->integer", "kawa.lib.characters");
      defProcStFld("integer->char", "kawa.lib.characters");
      defProcStFld("char-upcase", "kawa.lib.characters");
      defProcStFld("char-downcase", "kawa.lib.characters");
      
      //-- Section 6.7  -- complete
      defProcStFld("string?", "kawa.lib.strings");
      defProcStFld("make-string", "kawa.lib.strings");
      define_proc ("string", "kawa.standard.string_v");
      defProcStFld("string-length", "kawa.lib.strings");
      defProcStFld("string-ref", "kawa.lib.strings");
      defProcStFld("string-set!", "kawa.lib.strings");

      defProcStFld("string=?", "kawa.lib.strings");
      defProcStFld("string-ci=?", "kawa.lib.strings");
      defProcStFld("string<?", "kawa.lib.strings");
      defProcStFld("string>?", "kawa.lib.strings");
      defProcStFld("string<=?", "kawa.lib.strings");
      defProcStFld("string>=?", "kawa.lib.strings");

      defProcStFld("string-ci<?", "kawa.lib.strings");
      defProcStFld("string-ci>?", "kawa.lib.strings");
      defProcStFld("string-ci<=?", "kawa.lib.strings");
      defProcStFld("string-ci>=?", "kawa.lib.strings");

      defProcStFld("substring", "kawa.lib.strings");
      defProcStFld("string-append", "kawa.lib.strings");
      defProcStFld("string-append/shared", "kawa.lib.strings");
      defProcStFld("string->list", "kawa.lib.strings");
      defProcStFld("list->string", "kawa.lib.strings");
      defProcStFld("string-copy", "kawa.lib.strings");
      defProcStFld("string-fill!", "kawa.lib.strings");

      //-- Section 6.8  -- complete
      define_proc ("vector?", "kawa.lib.vectors");
      define_proc ("make-vector", "kawa.lib.vectors");
      define_proc ("vector", "kawa.lib.vectors");
      define_proc ("vector-length", "kawa.lib.vectors");
      define_field("vector-ref", "kawa.lib.vectors");
      define_proc ("vector-set!", "kawa.lib.vectors");
      define_proc ("list->vector", "kawa.lib.vectors");
      define_field("vector->list", "kawa.lib.vectors");
      define_proc ("vector-fill!", "kawa.lib.vectors");
      // Extension:
      define_field("vector-append", "kawa.standard.vector_append", "vectorAppend");
      define_field("values-append", "gnu.kawa.functions.AppendValues",
		   "appendValues");

      //-- Section 6.9  -- complete [except restricted call/cc]
      define_proc ("procedure?", "kawa.lib.misc");
      define_field("apply", "gnu.kawa.functions.Apply", "apply");
      define_field("map", "kawa.standard.Scheme", "map");
      define_field("for-each", "kawa.standard.Scheme", "forEach");
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");
      define_proc ("call/cc", "kawa.standard.callcc");
      define_proc ("force", "kawa.standard.force");

      //-- Section 6.10  -- complete
      defProcStFld("call-with-input-file", "kawa.lib.ports");
      defProcStFld("call-with-output-file", "kawa.lib.ports");
      defProcStFld("input-port?", "kawa.lib.ports");
      defProcStFld("output-port?", "kawa.lib.ports");
      defProcStFld("current-input-port", "kawa.lib.ports");
      defProcStFld("current-output-port", "kawa.lib.ports");
      define_proc ("with-input-from-file",
		   "kawa.standard.with_input_from_file");
      define_proc ("with-output-to-file",
		   "kawa.standard.with_output_to_file");
      defProcStFld("open-input-file", "kawa.lib.ports");
      defProcStFld("open-output-file", "kawa.lib.ports");
      defProcStFld("close-input-port", "kawa.lib.ports");
      defProcStFld("close-output-port", "kawa.lib.ports");
      define_proc ("read", "kawa.standard.read");
      define_proc ("read-line", "kawa.standard.read_line");
      defProcStFld("read-char", "kawa.standard.readchar", "readChar");
      defProcStFld("peek-char", "kawa.standard.readchar", "peekChar");
      defProcStFld("eof-object?", "kawa.lib.ports");
      defProcStFld("char-ready?", "kawa.lib.ports");
      defProcStFld("write", "kawa.standard.Scheme", "writeFormat");
      defProcStFld("display", "kawa.standard.Scheme", "displayFormat");
      defProcStFld("print-as-xml", "gnu.xquery.lang.XQuery", "writeFormat");
      defProcStFld("write-char", "kawa.lib.ports");
      defProcStFld("newline", "kawa.lib.ports");
      defProcStFld("load", "kawa.standard.load", "load");
      defProcStFld("load-relative", "kawa.standard.load", "loadRelative");
      defProcStFld("transcript-off", "kawa.lib.ports");
      defProcStFld("transcript-on", "kawa.lib.ports");
      defProcStFld("call-with-input-string", "kawa.lib.ports");  // Extension
      defProcStFld("open-input-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("open-output-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("get-output-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("call-with-output-string", "kawa.lib.ports"); // Extension
      defProcStFld("force-output", "kawa.lib.ports");  // Extension

      defProcStFld("port-line", "kawa.lib.ports");
      defProcStFld("set-port-line!", "kawa.lib.ports");
      defProcStFld("port-column", "kawa.lib.ports");
      defProcStFld("current-error-port", "kawa.lib.ports");
      defProcStFld("input-port-line-number", "kawa.lib.ports");  // Extension
      defProcStFld("set-input-port-line-number!", "kawa.lib.ports");
      defProcStFld("input-port-column-number", "kawa.lib.ports");
      defProcStFld("input-port-read-state", "kawa.lib.ports");
      defProcStFld("default-prompter", "kawa.lib.ports");
      defProcStFld("input-port-prompter", "kawa.lib.ports");
      defProcStFld("set-input-port-prompter!", "kawa.lib.ports");
      defProcStFld("base-uri", "kawa.lib.misc");

      define_syntax ("%syntax-error", "kawa.standard.syntax_error");

      r5Environment = new Environment (r4Environment);
      r5Environment.setName ("r5rs-environment");
      environ = r5Environment;
      define_proc ("values", "kawa.lib.misc");
      define_proc ("call-with-values", "kawa.standard.call_with_values");
      define_field("let-values", "kawa.lib.syntax");
      define_field("let*-values", "kawa.lib.syntax");
      define_field("receive", "kawa.lib.syntax");
      define_proc ("eval", "kawa.lang.Eval");
      define_proc ("repl", new kawa.repl(this));
      defProcStFld("scheme-report-environment", "kawa.lib.misc");
      defProcStFld("null-environment", "kawa.lib.misc");
      defProcStFld("interaction-environment", "kawa.lib.misc");
      defProcStFld("dynamic-wind", "kawa.lib.misc");

      kawaEnvironment = new Environment (r5Environment);
      environ = kawaEnvironment;
 
      define_syntax ("define-private", "kawa.lib.prim_syntax");
      define_syntax ("define-constant", "kawa.lib.prim_syntax");

      define_syntax("define-autoload", new define_autoload(false));
      define_syntax("define-autoloads-from-file", new define_autoload(true));

      define_proc ("exit", "kawa.lib.thread");

      define_field("arithmetic-shift", "kawa.lib.numbers");
      define_field("ash", "kawa.lib.numbers", "arithmetic$Mnshift");
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
      define_field("primitive-throw", "kawa.standard.prim_throw", "primitiveThrow");
      define_syntax("try-finally", "kawa.lib.syntax");
      define_syntax("try-catch", "kawa.lib.prim_syntax");
      define_proc("throw", "kawa.standard.throw_name");
      define_proc("catch", "kawa.lib.syntax");
      define_proc("error", "kawa.lib.misc");
      define_proc("as", gnu.kawa.functions.Convert.as);
      define_field("instance?", "kawa.standard.Scheme", "instanceOf");
      define_syntax("synchronized", "kawa.lib.syntax");
      object objectSyntax = new kawa.standard.object(lambda);
      define_syntax("object", objectSyntax);
      define_syntax("define-class",
                    new kawa.standard.define_class(objectSyntax, false));
      define_syntax("define-simple-class",
                    new kawa.standard.define_class(objectSyntax, true));
      define_syntax("this", "kawa.standard.thisRef");
      define_field("make", "gnu.kawa.reflect.Invoke", "make");
      define_field("slot-ref", "gnu.kawa.reflect.SlotGet", "field");
      define_field("slot-set!", "gnu.kawa.reflect.SlotSet", "setField$Ex");
      define_field("field", "gnu.kawa.reflect.SlotGet");
      define_proc("class-methods", "gnu.kawa.reflect.ClassMethods");
      define_field("static-field", "gnu.kawa.reflect.SlotGet",
		   "staticField");
      define_field("invoke", "gnu.kawa.reflect.Invoke", "invoke");

      define_field("invoke-static", "gnu.kawa.reflect.Invoke", "invokeStatic");
      define_field("invoke-special", "gnu.kawa.reflect.Invoke", "invokeSpecial");

      define ("define-macro", "kawa.lib.syntax");
      define ("%define-macro",
	      new kawa.standard.define_syntax("define-macro", false));
      define ("syntax-case", new kawa.standard.syntax_case ());

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
      
      define_proc ("record-accessor", "kawa.lib.reflection");
      define_proc ("record-modifier", "kawa.lib.reflection");
      define_proc ("record-predicate", "kawa.lib.reflection");
      define_proc ("record-constructor", "kawa.lib.reflection");
      define_proc ("make-record-type", "kawa.lib.reflection");
      define_proc ("record-type-descriptor", "kawa.lib.reflection");
      define_proc ("record-type-name", "kawa.lib.reflection");
      define_proc ("record-type-field-names", "kawa.lib.reflection");
      define_proc ("record?", "kawa.lib.reflection");
      define_syntax("define-record-type", "gnu.kawa.slib.DefineRecordType");

      define_syntax ("when", "kawa.lib.syntax"); //-- (when cond exp ...)
      define_syntax ("unless", "kawa.lib.syntax"); //-- (unless cond exp ...)
      define_syntax ("fluid-let", "kawa.standard.fluid_let");
      define_syntax("constant-fold", "kawa.standard.constant_fold");

      define_proc ("compile-file", "kawa.lib.system");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
      define_proc ("environment-bound?", "kawa.lib.misc");
      define_proc ("scheme-implementation-version", "kawa.lib.misc");
      define_proc ("scheme-window", "kawa.lib.windows");
      define_syntax ("define-procedure", "kawa.lib.syntax");
      define_field("make-procedure",
                   "gnu.kawa.functions.MakeProcedure", "makeProcedure");
      define_field("procedure-property", "kawa.lib.misc");
      define_proc ("set-procedure-property!", "kawa.lib.misc");

      define_proc ("quantity->number", "kawa.standard.quantity2number");
      define_proc ("quantity->unit", "kawa.standard.quantity2unit");
      define_proc ("make-quantity", "kawa.standard.make_quantity");
      define_syntax ("define-unit", new kawa.standard.define_unit(false));
      define_syntax ("define-namespace", "gnu.kawa.xml.DefineNamespace");
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

      define_field("make-element", "gnu.xquery.util.MakeElement", "makeElement");
      define_field("make-attribute", "gnu.xquery.util.MakeAttribute", "makeAttribute");
      define_field("map-values", "gnu.kawa.functions.ValuesMap", "valuesMap");
      define_field("children", "gnu.xquery.util.Children", "children");
      define_field("attributes", "gnu.kawa.xml.Attributes");
      define_field("unescaped-data", "gnu.kawa.xml.MakeUnescapedData",
		   "unescapedData");
      define_proc ("keyword?", "kawa.lib.keywords");
      define_proc ("keyword->string", "kawa.lib.keywords");
      define_proc ("string->keyword", "kawa.lib.keywords");
      define_syntax ("location", "kawa.standard.location");
      define ("define-alias", new kawa.standard.define_alias());
      define_syntax("define-variable", "kawa.standard.define_variable");
      define ("define-member-alias", new kawa.standard.define_member_alias());
      define ("require", new kawa.standard.require());
      define_syntax ("module-name", "kawa.standard.module_name");
      define_syntax ("module-extends", "kawa.standard.module_extends");
      define_syntax ("module-implements", "kawa.standard.module_implements");
      define_syntax ("module-static", "kawa.standard.module_static");
      define_syntax ("module-export", "kawa.standard.export");
      define_syntax ("module-compile-options",
		     "kawa.standard.module_compile_options");
      define_syntax ("with-compile-options",
		     "kawa.standard.with_compile_options");

      define_field("array?", "kawa.lib.arrays");
      define_field("array-rank", "kawa.lib.arrays");
      define_field("make-array", "kawa.lib.arrays");
      define_field("array", "kawa.lib.arrays");
      define_field("array-start", "kawa.lib.arrays");
      define_field("array-end", "kawa.lib.arrays");
      define_field("shape", "kawa.lib.arrays");
      define_field("array-ref", "gnu.kawa.functions.ArrayRef", "arrayRef");
      define_field("array-set!", "gnu.kawa.functions.ArraySet", "arraySet");
      define_field("share-array", "kawa.lib.arrays");

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

      define_syntax("cut", "gnu.kawa.slib.cut");
      define_syntax("cute", "gnu.kawa.slib.cut");

      define_proc ("emacs", "gnu.jemacs.buffer.emacs");
      define_proc ("node", "gnu.kawa.xml.MakeTreeNode");
      define_syntax("%if-feature", "kawa.standard.IfFeature");
      define_syntax("cond-expand", "kawa.lib.syntax");
  }

  static int scheme_counter = 0;

  public Scheme ()
  {
    if (Interpreter.defaultInterpreter == null)
      Interpreter.defaultInterpreter = this;
    environ = getNewEnvironment();
    if (instance == null)
      {
        instance = this;
        instanceOf = new gnu.kawa.reflect.InstanceOf(this, "instance?");
        not = new not(this, "not");
        map = new map(true);
	forEach = new map(false);
        isEq = new gnu.kawa.functions.IsEq(this, "eq?");
        isEqv = new gnu.kawa.functions.IsEqv(this, "eqv?", isEq);
        isEqual = new gnu.kawa.functions.IsEqual(this, "equal?");
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
    ScmEnv environ = new ScmEnv (kawaEnvironment);
    environ.setName ("interaction-environment."+(++scheme_counter));

    // Do: environ.addExtra(new NamespaceEnv(environ))
    // but without requiring that NamespaceEnv be available at compile-time.
    try
      {
	Class typeNamespaceEnv = Class.forName("gnu.kawa.xml.NamespaceEnv");
	Class[] argTypes = { Class.forName("gnu.mapping.Environment") };
	java.lang.reflect.Constructor constr
	  = typeNamespaceEnv.getDeclaredConstructor(argTypes);
	Environment[] args = { environ };
	environ.addExtra((Environment) constr.newInstance(args));
      }
    catch (Throwable ex)
      {
      }
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

  /** Evalutate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Interpreter.voidObject if none. */
  public static Object eval (InPort port, Environment env)
  {
    SourceMessages messages = new SourceMessages();
    try
      {
	LispReader lexer = (LispReader)
	  Interpreter.getInterpreter().getLexer(port, messages);
	Object body = ReaderParens.readList(lexer, 0, 1, -1);
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
    catch (java.io.IOException e)
      {
	throw new RuntimeException("eval: I/O exception: "
				   + e.toString ());
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  /** Evalutate Scheme expressions from an "S expression."
   * @param sexpr the S expression to evaluate
   * @param env the Environment to evaluate the string in
   * @return result of the expression. */
  public static Object eval (Object sexpr, Environment env)
  {
    try
      {
	return Eval.eval (sexpr, env);
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
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

  public static final DisplayFormat writeFormat = new DisplayFormat(true, 'S');
  public static final DisplayFormat displayFormat = new DisplayFormat(false, 'S');

  public FormatToConsumer getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  public int getNamespaceOf (Declaration decl)
  {
    return FUNCTION_NAMESPACE+VALUE_NAMESPACE;
  }

  /** If exp is a "constant" Type, return that type, otherwise return null. */
  public static Type getTypeValue (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  static Hashtable types;

  public static Type getNamedType (String name)
  {
    if (types == null)
      {
	booleanType
	  = new LangPrimType(Type.boolean_type, Scheme.getInstance());
	types = new Hashtable ();
	types.put ("void", LangPrimType.voidType);
	types.put ("int", LangPrimType.intType);
	types.put ("char", LangPrimType.charType);
	types.put ("boolean", booleanType);
	types.put ("byte", LangPrimType.byteType);
	types.put ("short", LangPrimType.shortType);
	types.put ("long", LangPrimType.longType);
	types.put ("float", LangPrimType.floatType);
	types.put ("double", LangPrimType.doubleType);
	types.put ("never-returns", Type.neverReturnsType);

	types.put ("Object", Type.pointer_type);
	types.put ("java.lang.Object", Type.pointer_type);
	types.put ("String", Type.tostring_type);

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
	types.put ("abstract-string", ClassType.make("gnu.lists.CharSeq"));
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
        types.put ("readtable", ClassType.make("gnu.kawa.lispexpr.ReadTable"));
      }
    Type type = (Type) types.get(name);
    if (type == null
	&& (name.startsWith("elisp:") || name.startsWith("clisp:")))
      {
	int colon = name.indexOf(':');
	Class clas = getNamedType(name.substring(colon+1)).getReflectClass();
	String lang = name.substring(0,colon);
	Interpreter interp = Interpreter.getInstance(lang);
	if (interp == null)
	    throw new RuntimeException("unknown type '" + name
				       + "' - unknown language '"
				       + lang + '\'');
	type = interp.getTypeFor(clas);
	if (type != null)
	  types.put(name, type);
      }
    return type;
  }

  public Type getTypeFor (Class clas)
  {
    String name = clas.getName();
    if (clas.isPrimitive())
      return getNamedType(name);
    if ("java.lang.String".equals(name))
      return Type.tostring_type;
    return Type.make(clas);
  }

  public static Type string2Type (String name)
  {
    Type t;
    if (name.endsWith("[]"))
      {
	t = string2Type(name.substring(0, name.length()-2));
	if (t != null)
	  t = ArrayType.make(t);
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
    Environment.setGlobal(interp.getEnvironment());
  }
}
