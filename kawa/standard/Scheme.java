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

  /** Declare in the current Environment a Syntax bound to a static field.
   * @param name the procedure's source-level name.
   * @param cname the name of the class containing the field.
   */

  protected void defSntxStFld(String name, String className)
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
      defSntxStFld("define", "kawa.lib.prim_syntax");

      defSntxStFld("if", "kawa.lib.prim_syntax");
      define_syntax ("set!", "kawa.standard.set_b");

      // Section 4.2  -- complete
      define_syntax ("cond", "kawa.lib.std_syntax");
      define_syntax ("case", "kawa.lib.std_syntax");
      define_syntax ("and", "kawa.lib.std_syntax");
      define_syntax ("or", "kawa.lib.std_syntax");
      define_syntax ("%let", kawa.standard.let.let);
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
      defProcStFld("not", "kawa.standard.Scheme");
      defProcStFld("boolean?", "kawa.lib.misc");

      //-- Section 6.2  -- complete
      defProcStFld("eq?", "kawa.standard.Scheme", "isEq");
      defProcStFld("eqv?", "kawa.standard.Scheme", "isEqv");
      defProcStFld("equal?", "kawa.standard.Scheme", "isEqual");

      //-- Section 6.3  -- complete
      defProcStFld("pair?", "kawa.lib.lists");
      defProcStFld("cons", "kawa.lib.lists");
      defProcStFld("car", "kawa.lib.lists");
      defProcStFld("cdr", "kawa.lib.lists");
      defProcStFld("set-car!", "kawa.lib.lists");
      defProcStFld("set-cdr!", "kawa.lib.lists");

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
      defProcStFld("null?", "kawa.lib.lists");
      defProcStFld("list?", "kawa.lib.lists");
      defProcStFld("list", "gnu.kawa.functions.MakeList");
      defProcStFld("length", "kawa.lib.lists");
      defProcStFld("append", "kawa.standard.append", "append");
      defProcStFld("reverse", "kawa.lib.lists");
      defProcStFld("reverse!", "kawa.lib.lists");  // Not R5RS.
      defProcStFld("list-tail", "kawa.lib.lists");
      defProcStFld("list-ref", "kawa.lib.lists");

      defProcStFld("memq", "kawa.lib.lists");
      defProcStFld("memv", "kawa.lib.lists");
      defProcStFld("member", "kawa.lib.lists");
      defProcStFld("assq", "kawa.lib.lists");
      defProcStFld("assv", "kawa.lib.lists");
      defProcStFld("assoc", "kawa.lib.lists");

      //-- Section 6.4  -- complete, including slashified read/write
      
      defProcStFld("symbol?", "kawa.lib.misc");
      defProcStFld("symbol->string", "kawa.lib.misc");
      defProcStFld("string->symbol", "kawa.lib.misc");

      //-- Section 6.5
      defProcStFld("number?", "kawa.lib.numbers");
      defProcStFld("quantity?", "kawa.lib.numbers");
      defProcStFld("complex?", "kawa.lib.numbers");
      defProcStFld("real?", "kawa.lib.numbers");
      defProcStFld("rational?", "kawa.lib.numbers");
      define_proc ("integer?", "kawa.standard.integer_p");
      defProcStFld("exact?", "kawa.lib.numbers");
      defProcStFld("inexact?", "kawa.lib.numbers");
      defProcStFld("=", "gnu.kawa.functions.NumberCompare", "$Eq");
      defProcStFld("<", "gnu.kawa.functions.NumberCompare", "$Ls");
      defProcStFld(">", "gnu.kawa.functions.NumberCompare", "$Gr");
      defProcStFld("<=", "gnu.kawa.functions.NumberCompare", "$Ls$Eq");
      defProcStFld(">=", "gnu.kawa.functions.NumberCompare", "$Gr$Eq");
      defProcStFld("zero?", "kawa.lib.numbers");
      defProcStFld("positive?", "kawa.lib.numbers");
      defProcStFld("negative?", "kawa.lib.numbers");
      defProcStFld("odd?", "kawa.lib.numbers");
      defProcStFld("even?", "kawa.lib.numbers");
      define_proc ("max", "kawa.standard.max");
      define_proc ("min", "kawa.standard.min");
      defProcStFld("+", "gnu.kawa.functions.AddOp", "$Pl");
      defProcStFld("-", "gnu.kawa.functions.AddOp", "$Mn");
      defProcStFld("*", "gnu.kawa.functions.MultiplyOp", "$St");
      defProcStFld("/", "gnu.kawa.functions.DivideOp", "$Sl");
      defProcStFld("abs", "kawa.lib.numbers");
      defProcStFld("quotient", "kawa.lib.numbers");
      defProcStFld("remainder", "kawa.lib.numbers");
      defProcStFld("modulo", "kawa.lib.numbers");
      define_proc ("gcd", "kawa.standard.gcd");
      define_proc ("lcm", "kawa.standard.lcm");
      defProcStFld("numerator", "kawa.lib.numbers");
      defProcStFld("denominator", "kawa.lib.numbers");
      defProcStFld("floor", "kawa.lib.numbers");
      defProcStFld("ceiling", "kawa.lib.numbers");
      defProcStFld("truncate", "kawa.lib.numbers");
      defProcStFld("round", "kawa.lib.numbers");
      defProcStFld("rationalize", "kawa.lib.numbers");
      defProcStFld("exp", "kawa.lib.numbers");
      defProcStFld("log", "kawa.lib.numbers");
      defProcStFld("sin", "kawa.lib.numbers");
      defProcStFld("cos", "kawa.lib.numbers");
      defProcStFld("tan", "kawa.lib.numbers");
      defProcStFld("asin", "kawa.lib.numbers");
      defProcStFld("acos", "kawa.lib.numbers");
      define_proc ("atan", "kawa.standard.atan");
      defProcStFld("sqrt", "kawa.lib.numbers");
      defProcStFld("expt", "kawa.standard.expt");
      defProcStFld("make-rectangular", "kawa.lib.numbers");
      defProcStFld("make-polar", "kawa.lib.numbers");
      defProcStFld("real-part", "kawa.lib.numbers");
      defProcStFld("imag-part", "kawa.lib.numbers");
      defProcStFld("magnitude", "kawa.lib.numbers");
      defProcStFld("angle", "kawa.lib.numbers");
      defProcStFld("exact->inexact", "kawa.lib.numbers");
      defProcStFld("inexact->exact", "kawa.lib.numbers");
      defProcStFld("number->string", "kawa.lib.numbers");
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
      defProcStFld("vector?", "kawa.lib.vectors");
      defProcStFld("make-vector", "kawa.lib.vectors");
      defProcStFld("vector", "kawa.lib.vectors");
      defProcStFld("vector-length", "kawa.lib.vectors");
      defProcStFld("vector-ref", "kawa.lib.vectors");
      defProcStFld("vector-set!", "kawa.lib.vectors");
      defProcStFld("list->vector", "kawa.lib.vectors");
      defProcStFld("vector->list", "kawa.lib.vectors");
      defProcStFld("vector-fill!", "kawa.lib.vectors");
      // Extension:
      defProcStFld("vector-append", "kawa.standard.vector_append", "vectorAppend");
      defProcStFld("values-append", "gnu.kawa.functions.AppendValues",
		   "appendValues");

      //-- Section 6.9  -- complete [except restricted call/cc]
      defProcStFld("procedure?", "kawa.lib.misc");
      defProcStFld("apply", "gnu.kawa.functions.Apply", "apply");
      defProcStFld("map", "kawa.standard.Scheme", "map");
      defProcStFld("for-each", "kawa.standard.Scheme", "forEach");
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");
      define_proc ("call/cc", "kawa.standard.callcc");
      defProcStFld("force", "kawa.lib.misc");

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
      defProcStFld("values", "kawa.lib.misc");
      defProcStFld("call-with-values", "kawa.standard.call_with_values",
		   "callWithValues");
      defSntxStFld("let-values", "kawa.lib.syntax");
      defSntxStFld("let*-values", "kawa.lib.syntax");
      defSntxStFld("receive", "kawa.lib.syntax");
      define_proc ("eval", "kawa.lang.Eval");
      define_proc ("repl", new kawa.repl(this));
      defProcStFld("scheme-report-environment", "kawa.lib.misc");
      defProcStFld("null-environment", "kawa.lib.misc");
      defProcStFld("interaction-environment", "kawa.lib.misc");
      defProcStFld("dynamic-wind", "kawa.lib.misc");

      kawaEnvironment = new Environment (r5Environment);
      environ = kawaEnvironment;
 
      defSntxStFld ("define-private", "kawa.lib.prim_syntax");
      defSntxStFld ("define-constant", "kawa.lib.prim_syntax");

      define_syntax("define-autoload", new define_autoload(false));
      define_syntax("define-autoloads-from-file", new define_autoload(true));

      defProcStFld("exit", "kawa.lib.thread");

      defProcStFld("arithmetic-shift", "kawa.lib.numbers");
      defProcStFld("ash", "kawa.lib.numbers", "arithmetic$Mnshift");
      define_proc ("logand", "kawa.standard.logand");
      define_proc ("logior", "kawa.standard.logior");
      define_proc ("logxor", "kawa.standard.logxor");
      defProcStFld("lognot", "kawa.lib.numbers");
      defProcStFld("logop", "kawa.lib.numbers");
      defProcStFld("logbit?", "kawa.lib.numbers");
      defProcStFld("logtest", "kawa.lib.numbers");
      defProcStFld("logcount", "kawa.lib.numbers");
      defProcStFld("bit-extract", "kawa.lib.numbers");
      defProcStFld("integer-length", "kawa.lib.numbers");

      // These are from SLIB.
      defProcStFld("string-upcase!", "kawa.lib.strings");
      defProcStFld("string-downcase!", "kawa.lib.strings");
      defProcStFld("string-capitalize!", "kawa.lib.strings");
      defProcStFld("string-upcase", "kawa.lib.strings");
      defProcStFld("string-downcase", "kawa.lib.strings");
      defProcStFld("string-capitalize", "kawa.lib.strings");
      define_syntax("primitive-virtual-method",
                    new kawa.standard.prim_method(182));
      define_syntax("primitive-static-method",
                    new kawa.standard.prim_method(184));
      define_syntax("primitive-interface-method",
                    new kawa.standard.prim_method(185));
      define_syntax("primitive-constructor",
                    new kawa.standard.prim_method(183));
      define_syntax("primitive-op1", new kawa.standard.prim_method());
      defSntxStFld("primitive-get-field", "kawa.lib.reflection");
      defSntxStFld("primitive-set-field", "kawa.lib.reflection");
      defSntxStFld("primitive-get-static", "kawa.lib.reflection");
      defSntxStFld("primitive-set-static", "kawa.lib.reflection");
      defSntxStFld("primitive-array-new", "kawa.lib.reflection");
      defSntxStFld("primitive-array-get", "kawa.lib.reflection");
      defSntxStFld("primitive-array-set", "kawa.lib.reflection");
      defSntxStFld("primitive-array-length", "kawa.lib.reflection");
      defProcStFld("subtype?", "kawa.lib.reflection");
      defProcStFld("primitive-throw", "kawa.standard.prim_throw", "primitiveThrow");
      defSntxStFld("try-finally", "kawa.lib.syntax");
      defSntxStFld("try-catch", "kawa.lib.prim_syntax");
      defProcStFld("throw", "kawa.standard.throw_name", "throwName");
      defProcStFld("catch", "kawa.lib.syntax");
      defProcStFld("error", "kawa.lib.misc");
      defProcStFld("as", "gnu.kawa.functions.Convert", "as");
      defProcStFld("instance?", "kawa.standard.Scheme", "instanceOf");
      defSntxStFld("synchronized", "kawa.lib.syntax");
      object objectSyntax = new kawa.standard.object(lambda);
      define_syntax("object", objectSyntax);
      define_syntax("define-class",
                    new kawa.standard.define_class(objectSyntax, false));
      define_syntax("define-simple-class",
                    new kawa.standard.define_class(objectSyntax, true));
      define_syntax("this", "kawa.standard.thisRef");
      defProcStFld("make", "gnu.kawa.reflect.Invoke", "make");
      defProcStFld("slot-ref", "gnu.kawa.reflect.SlotGet", "field");
      defProcStFld("slot-set!", "gnu.kawa.reflect.SlotSet", "setField$Ex");
      defProcStFld("field", "gnu.kawa.reflect.SlotGet");
      defProcStFld("class-methods", "gnu.kawa.reflect.ClassMethods");
      defProcStFld("static-field", "gnu.kawa.reflect.SlotGet",
		   "staticField");
      defProcStFld("invoke", "gnu.kawa.reflect.Invoke", "invoke");

      defProcStFld("invoke-static", "gnu.kawa.reflect.Invoke", "invokeStatic");
      defProcStFld("invoke-special", "gnu.kawa.reflect.Invoke", "invokeSpecial");

      define ("define-macro", "kawa.lib.syntax");
      define ("%define-macro",
	      new kawa.standard.define_syntax("define-macro", false));
      define ("syntax-case", new kawa.standard.syntax_case ());

      defProcStFld("file-exists?", "kawa.lib.files");
      defProcStFld("file-directory?", "kawa.lib.files");
      defProcStFld("file-readable?", "kawa.lib.files");
      defProcStFld("file-writable?", "kawa.lib.files");
      defProcStFld("delete-file", "kawa.lib.files");
      defProcStFld("system-tmpdir", "kawa.lib.files");
      defProcStFld("make-temporary-file", "kawa.lib.files");
      defProcStFld("rename-file", "kawa.lib.files");
      defProcStFld("copy-file", "kawa.lib.files");
      defProcStFld("create-directory", "kawa.lib.files");
      defProcStFld("->pathname", "kawa.lib.files");
      define("port-char-encoding", Boolean.TRUE);
      define("symbol-read-case", "P");

      defProcStFld("system", "kawa.lib.system");
      defProcStFld("make-process", "kawa.lib.system");
      define_proc("tokenize-string-to-string-array", "kawa.lib.system");
      define_proc("tokenize-string-using-shell", "kawa.lib.system");
      if ("/".equals(System.getProperty("file.separator")))
	define ("command-parse", lookup("tokenize-string-using-shell"));
      else
	define ("command-parse", lookup("tokenize-string-to-string-array"));
      
      defProcStFld("record-accessor", "kawa.lib.reflection");
      defProcStFld("record-modifier", "kawa.lib.reflection");
      defProcStFld("record-predicate", "kawa.lib.reflection");
      defProcStFld("record-constructor", "kawa.lib.reflection");
      defProcStFld("make-record-type", "kawa.lib.reflection");
      defProcStFld("record-type-descriptor", "kawa.lib.reflection");
      defProcStFld("record-type-name", "kawa.lib.reflection");
      defProcStFld("record-type-field-names", "kawa.lib.reflection");
      defProcStFld("record?", "kawa.lib.reflection");
      defSntxStFld("define-record-type", "gnu.kawa.slib.DefineRecordType");

      defSntxStFld("when", "kawa.lib.syntax"); //-- (when cond exp ...)
      defSntxStFld("unless", "kawa.lib.syntax"); //-- (unless cond exp ...)
      define_syntax ("fluid-let", "kawa.standard.fluid_let");
      define_syntax("constant-fold", "kawa.standard.constant_fold");

      defProcStFld("compile-file", "kawa.lib.system");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
      defProcStFld("environment-bound?", "kawa.lib.misc");
      defProcStFld("scheme-implementation-version", "kawa.lib.misc");
      defProcStFld("scheme-window", "kawa.lib.windows");
      defSntxStFld("define-procedure", "kawa.lib.syntax");
      defProcStFld("make-procedure",
                   "gnu.kawa.functions.MakeProcedure", "makeProcedure");
      defProcStFld("procedure-property", "kawa.lib.misc");
      defProcStFld("set-procedure-property!", "kawa.lib.misc");

      define_proc ("quantity->number", "kawa.standard.quantity2number");
      define_proc ("quantity->unit", "kawa.standard.quantity2unit");
      define_proc ("make-quantity", "kawa.standard.make_quantity");
      define_syntax ("define-unit", new kawa.standard.define_unit(false));
      define_syntax ("define-namespace", "gnu.kawa.xml.DefineNamespace");
      define_syntax ("define-base-unit", new kawa.standard.define_unit(true));

      defProcStFld("gentemp", "kawa.lib.syntax");
      defSntxStFld("defmacro", "kawa.lib.syntax");
      defProcStFld("setter", "gnu.kawa.functions.Setter", "setter");

      defSntxStFld("future", "kawa.lib.thread");
      define_proc ("sleep", "kawa.standard.sleep");

      defSntxStFld("trace", "kawa.lib.trace");
      defSntxStFld("untrace", "kawa.lib.trace");

      defProcStFld("format", "gnu.kawa.functions.Format");
      defProcStFld("parse-format", "gnu.kawa.functions.ParseFormat", "parseFormat");

      defProcStFld("make-element", "gnu.xquery.util.MakeElement", "makeElement");
      defProcStFld("make-attribute", "gnu.xquery.util.MakeAttribute", "makeAttribute");
      defProcStFld("map-values", "gnu.kawa.functions.ValuesMap", "valuesMap");
      defProcStFld("children", "gnu.xquery.util.Children", "children");
      defProcStFld("attributes", "gnu.kawa.xml.Attributes");
      defProcStFld("unescaped-data", "gnu.kawa.xml.MakeUnescapedData",
		   "unescapedData");
      defProcStFld("keyword?", "kawa.lib.keywords");
      defProcStFld("keyword->string", "kawa.lib.keywords");
      defProcStFld("string->keyword", "kawa.lib.keywords");
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

      defProcStFld("array?", "kawa.lib.arrays");
      defProcStFld("array-rank", "kawa.lib.arrays");
      defProcStFld("make-array", "kawa.lib.arrays");
      defProcStFld("array", "kawa.lib.arrays");
      defProcStFld("array-start", "kawa.lib.arrays");
      defProcStFld("array-end", "kawa.lib.arrays");
      defProcStFld("shape", "kawa.lib.arrays");
      defProcStFld("array-ref", "gnu.kawa.functions.ArrayRef", "arrayRef");
      defProcStFld("array-set!", "gnu.kawa.functions.ArraySet", "arraySet");
      defProcStFld("share-array", "kawa.lib.arrays");

      defProcStFld("s8vector?", "kawa.lib.uniform");
      defProcStFld("make-s8vector", "kawa.lib.uniform");
      defProcStFld("s8vector", "kawa.lib.uniform");
      defProcStFld("s8vector-length", "kawa.lib.uniform");
      defProcStFld("s8vector-ref", "kawa.lib.uniform");
      defProcStFld("s8vector-set!", "kawa.lib.uniform");
      defProcStFld("s8vector->list", "kawa.lib.uniform");
      defProcStFld("list->s8vector", "kawa.lib.uniform");
      defProcStFld("u8vector?", "kawa.lib.uniform");
      defProcStFld("make-u8vector", "kawa.lib.uniform");
      defProcStFld("u8vector", "kawa.lib.uniform");
      defProcStFld("u8vector-length", "kawa.lib.uniform");
      defProcStFld("u8vector-ref", "kawa.lib.uniform");
      defProcStFld("u8vector-set!", "kawa.lib.uniform");
      defProcStFld("u8vector->list", "kawa.lib.uniform");
      defProcStFld("list->u8vector", "kawa.lib.uniform");

      defProcStFld("s16vector?", "kawa.lib.uniform");
      defProcStFld("make-s16vector", "kawa.lib.uniform");
      defProcStFld("s16vector", "kawa.lib.uniform");
      defProcStFld("s16vector-length", "kawa.lib.uniform");
      defProcStFld("s16vector-ref", "kawa.lib.uniform");
      defProcStFld("s16vector-set!", "kawa.lib.uniform");
      defProcStFld("s16vector->list", "kawa.lib.uniform");
      defProcStFld("list->s16vector", "kawa.lib.uniform");
      defProcStFld("u16vector?", "kawa.lib.uniform");
      defProcStFld("make-u16vector", "kawa.lib.uniform");
      defProcStFld("u16vector", "kawa.lib.uniform");
      defProcStFld("u16vector-length", "kawa.lib.uniform");
      defProcStFld("u16vector-ref", "kawa.lib.uniform");
      defProcStFld("u16vector-set!", "kawa.lib.uniform");
      defProcStFld("u16vector->list", "kawa.lib.uniform");
      defProcStFld("list->u16vector", "kawa.lib.uniform");

      defProcStFld("s32vector?", "kawa.lib.uniform");
      defProcStFld("make-s32vector", "kawa.lib.uniform");
      defProcStFld("s32vector", "kawa.lib.uniform");
      defProcStFld("s32vector-length", "kawa.lib.uniform");
      defProcStFld("s32vector-ref", "kawa.lib.uniform");
      defProcStFld("s32vector-set!", "kawa.lib.uniform");
      defProcStFld("s32vector->list", "kawa.lib.uniform");
      defProcStFld("list->s32vector", "kawa.lib.uniform");
      defProcStFld("u32vector?", "kawa.lib.uniform");
      defProcStFld("make-u32vector", "kawa.lib.uniform");
      defProcStFld("u32vector", "kawa.lib.uniform");
      defProcStFld("u32vector-length", "kawa.lib.uniform");
      defProcStFld("u32vector-ref", "kawa.lib.uniform");
      defProcStFld("u32vector-set!", "kawa.lib.uniform");
      defProcStFld("u32vector->list", "kawa.lib.uniform");
      defProcStFld("list->u32vector", "kawa.lib.uniform");

      defProcStFld("s64vector?", "kawa.lib.uniform");
      defProcStFld("make-s64vector", "kawa.lib.uniform");
      defProcStFld("s64vector", "kawa.lib.uniform");
      defProcStFld("s64vector-length", "kawa.lib.uniform");
      defProcStFld("s64vector-ref", "kawa.lib.uniform");
      defProcStFld("s64vector-set!", "kawa.lib.uniform");
      defProcStFld("s64vector->list", "kawa.lib.uniform");
      defProcStFld("list->s64vector", "kawa.lib.uniform");
      defProcStFld("u64vector?", "kawa.lib.uniform");
      defProcStFld("make-u64vector", "kawa.lib.uniform");
      defProcStFld("u64vector", "kawa.lib.uniform");
      defProcStFld("u64vector-length", "kawa.lib.uniform");
      defProcStFld("u64vector-ref", "kawa.lib.uniform");
      defProcStFld("u64vector-set!", "kawa.lib.uniform");
      defProcStFld("u64vector->list", "kawa.lib.uniform");
      defProcStFld("list->u64vector", "kawa.lib.uniform");

      defProcStFld("f32vector?", "kawa.lib.uniform");
      defProcStFld("make-f32vector", "kawa.lib.uniform");
      defProcStFld("f32vector", "kawa.lib.uniform");
      defProcStFld("f32vector-length", "kawa.lib.uniform");
      defProcStFld("f32vector-ref", "kawa.lib.uniform");
      defProcStFld("f32vector-set!", "kawa.lib.uniform");
      defProcStFld("f32vector->list", "kawa.lib.uniform");
      defProcStFld("list->f32vector", "kawa.lib.uniform");
      defProcStFld("f64vector?", "kawa.lib.uniform");
      defProcStFld("make-f64vector", "kawa.lib.uniform");
      defProcStFld("f64vector", "kawa.lib.uniform");
      defProcStFld("f64vector-length", "kawa.lib.uniform");
      defProcStFld("f64vector-ref", "kawa.lib.uniform");
      defProcStFld("f64vector-set!", "kawa.lib.uniform");
      defProcStFld("f64vector->list", "kawa.lib.uniform");
      defProcStFld("list->f64vector", "kawa.lib.uniform");

      defSntxStFld("cut", "gnu.kawa.slib.cut");
      defSntxStFld("cute", "gnu.kawa.slib.cut");

      define_proc ("emacs", "gnu.jemacs.buffer.emacs");
      define_proc ("node", "gnu.kawa.xml.MakeTreeNode");
      defSntxStFld("cond-expand", "kawa.lib.syntax");
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
