package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.ArrayType;
import gnu.mapping.*;
import gnu.math.IntNum;
import gnu.expr.*;
import java.util.*;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.kawa.lispexpr.*;
import gnu.lists.AbstractFormat;
import gnu.kawa.functions.*;
import gnu.kawa.reflect.ClassMethods;
import gnu.kawa.reflect.StaticFieldLocation;
import gnu.math.DFloNum;
import gnu.kawa.xml.XmlNamespace;
import gnu.math.Unit;
import gnu.kawa.servlet.HttpRequestContext;

public class Scheme extends LispLanguage
{
  public static final Environment nullEnvironment;
  public static final Environment r4Environment;
  public static final Environment r5Environment;
  protected static final SimpleEnvironment kawaEnvironment;

  public static LangPrimType booleanType;
  public static final Scheme instance;

  public static final gnu.kawa.reflect.InstanceOf instanceOf;
  public static final Not not;
  public static final gnu.kawa.functions.Map map;
  public static final gnu.kawa.functions.Map forEach;
  public static final gnu.kawa.functions.IsEq isEq;
  public static final gnu.kawa.functions.IsEqv isEqv;
  public static final gnu.kawa.functions.IsEqual isEqual;

  public static final NumberCompare numEqu;
  public static final NumberCompare numGrt;
  public static final NumberCompare numGEq;
  public static final NumberCompare numLss;
  public static final NumberCompare numLEq;
  public static final NumberPredicate isOdd;
  public static final NumberPredicate isEven;

  public static final Apply apply;
  public static final ApplyToArgs applyToArgs;
  static final Declaration applyFieldDecl;

  static {
    // (null-environment)
    nullEnvironment = Environment.make("null-environment");
    r4Environment = Environment.make("r4rs-environment", nullEnvironment);
    r5Environment = Environment.make("r5rs-environment", r4Environment);
    kawaEnvironment = Environment.make("kawa-environment", r5Environment);

    instance = new Scheme(kawaEnvironment);
    instanceOf = new gnu.kawa.reflect.InstanceOf(instance, "instance?");
    not = new Not(instance, "not");
    applyToArgs = new ApplyToArgs("apply-to-args", instance);
    applyFieldDecl
      = Declaration.getDeclarationFromStatic("kawa.standard.Scheme",
                                             "applyToArgs");
    apply = new Apply("apply", applyToArgs);
    isEq = new gnu.kawa.functions.IsEq(instance, "eq?");
    isEqv = new gnu.kawa.functions.IsEqv(instance, "eqv?", isEq);
    isEqual = new gnu.kawa.functions.IsEqual(instance, "equal?");
    map = new gnu.kawa.functions.Map(true, applyToArgs, applyFieldDecl, isEq);
    forEach = new gnu.kawa.functions.Map(false, applyToArgs, applyFieldDecl, isEq);
    numEqu = NumberCompare.make(instance, "=",
                                NumberCompare.TRUE_IF_EQU);
    numGrt = NumberCompare.make(instance, ">",
                                NumberCompare.TRUE_IF_GRT);
    numGEq = NumberCompare.make(instance, ">=",
                                NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU);
    numLss = NumberCompare.make(instance, "<",
                                NumberCompare.TRUE_IF_LSS);
    numLEq = NumberCompare.make(instance, "<=",
                                NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU);
    isOdd = new NumberPredicate(instance, "odd?", NumberPredicate.ODD);
    isEven = new NumberPredicate(instance, "even?", NumberPredicate.EVEN);

    instance.initScheme();

    int withServlets = HttpRequestContext.importServletDefinitions;
    if (withServlets > 0)
      {
        try
          {
            instance.loadClass(withServlets > 1 ? "gnu.kawa.servlet.servlets"
                               : "gnu.kawa.servlet.HTTP");
          }
        catch (Throwable ex)
          {
          }
      }
  }

  public static Scheme getInstance()
  {
    return instance;
  }

  public static Environment builtin ()
  {
    return kawaEnvironment;
  }

  private void initScheme ()
  {
      environ = nullEnvironment;

      environ.addLocation(LispLanguage.lookup_sym, null, getNamedPartLocation);

      defSntxStFld("lambda", "kawa.standard.SchemeCompilation", "lambda");

      //-- Section 4.1  -- complete
      defSntxStFld(LispLanguage.quote_sym, "kawa.lang.Quote", "plainQuote");
      defSntxStFld("%define", "kawa.standard.define", "defineRaw");
      defSntxStFld("define", "kawa.lib.prim_syntax");

      defSntxStFld("if", "kawa.lib.prim_syntax");
      defSntxStFld ("set!", "kawa.standard.set_b", "set");

      // Section 4.2  -- complete
      defSntxStFld("cond", "kawa.lib.std_syntax");
      defSntxStFld("case", "kawa.lib.std_syntax");
      defSntxStFld("and", "kawa.lib.std_syntax");
      defSntxStFld("or", "kawa.lib.std_syntax");
      defSntxStFld("%let", "kawa.standard.let", "let");
      defSntxStFld("let", "kawa.lib.std_syntax");
      defSntxStFld("let*", "kawa.lib.std_syntax");
      defSntxStFld("letrec", "kawa.lib.prim_syntax");

      defSntxStFld("begin", "kawa.standard.begin", "begin");
      defSntxStFld("do", "kawa.lib.std_syntax");
      defSntxStFld("delay", "kawa.lib.std_syntax");
      defProcStFld("%make-promise", "kawa.lib.std_syntax");
      defSntxStFld("quasiquote", "kawa.lang.Quote", "quasiQuote");

      //-- Section 5  -- complete [except for internal definitions]

      // Appendix (and R5RS)
      defSntxStFld("define-syntax", "kawa.lib.prim_syntax");
      defSntxStFld("let-syntax", "kawa.standard.let_syntax", "let_syntax");
      defSntxStFld("letrec-syntax", "kawa.standard.let_syntax",
                   "letrec_syntax");
      defSntxStFld("syntax-rules", "kawa.standard.syntax_rules",
                   "syntax_rules");

      nullEnvironment.setLocked();
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

      defProcStFld("caar", "kawa.lib.lists");
      defProcStFld("cadr", "kawa.lib.lists");
      defProcStFld("cdar", "kawa.lib.lists");
      defProcStFld("cddr", "kawa.lib.lists");
      defProcStFld("caaar", "kawa.lib.lists");
      defProcStFld("caadr", "kawa.lib.lists");
      defProcStFld("cadar", "kawa.lib.lists");
      defProcStFld("caddr", "kawa.lib.lists");
      defProcStFld("cdaar", "kawa.lib.lists");
      defProcStFld("cdadr", "kawa.lib.lists");
      defProcStFld("cddar", "kawa.lib.lists");
      defProcStFld("cdddr", "kawa.lib.lists");
      defProcStFld("caaaar", "kawa.lib.lists");
      defProcStFld("caaadr", "kawa.lib.lists");
      defProcStFld("caadar", "kawa.lib.lists");
      defProcStFld("caaddr", "kawa.lib.lists");
      defProcStFld("cadaar", "kawa.lib.lists");
      defProcStFld("cadadr", "kawa.lib.lists");
      defProcStFld("caddar", "kawa.lib.lists");
      defProcStFld("cadddr", "kawa.lib.lists");
      defProcStFld("cdaaar", "kawa.lib.lists");
      defProcStFld("cdaadr", "kawa.lib.lists");
      defProcStFld("cdadar", "kawa.lib.lists");
      defProcStFld("cdaddr", "kawa.lib.lists");
      defProcStFld("cddaar", "kawa.lib.lists");
      defProcStFld("cddadr", "kawa.lib.lists");
      defProcStFld("cdddar", "kawa.lib.lists");
      defProcStFld("cddddr", "kawa.lib.lists");
      defProcStFld("null?", "kawa.lib.lists");
      defProcStFld("list?", "kawa.lib.lists");
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
      defProcStFld("symbol=?", "kawa.lib.misc");
      defProcStFld("symbol-local-name", "kawa.lib.misc");
      defProcStFld("symbol-namespace", "kawa.lib.misc");
      defProcStFld("symbol-namespace-uri", "kawa.lib.misc");
      defProcStFld("symbol-prefix", "kawa.lib.misc");
      defProcStFld("namespace-uri", "kawa.lib.misc");
      defProcStFld("namespace-prefix", "kawa.lib.misc");

      //-- Section 6.5
      defProcStFld("number?", "kawa.lib.numbers");
      defProcStFld("quantity?", "kawa.lib.numbers");
      defProcStFld("complex?", "kawa.lib.numbers");
      defProcStFld("real?", "kawa.lib.numbers");
      defProcStFld("rational?", "kawa.lib.numbers");
      defProcStFld("integer?", "kawa.lib.numbers");
      defProcStFld("exact?", "kawa.lib.numbers");
      defProcStFld("inexact?", "kawa.lib.numbers");
      defProcStFld("=", "kawa.standard.Scheme", "numEqu");
      defProcStFld("<", "kawa.standard.Scheme", "numLss");
      defProcStFld(">", "kawa.standard.Scheme", "numGrt");
      defProcStFld("<=", "kawa.standard.Scheme", "numLEq");
      defProcStFld(">=", "kawa.standard.Scheme", "numGEq");
      defProcStFld("zero?", "kawa.lib.numbers");
      defProcStFld("positive?", "kawa.lib.numbers");
      defProcStFld("negative?", "kawa.lib.numbers");
      defProcStFld("odd?", "kawa.standard.Scheme", "isOdd");
      defProcStFld("even?", "kawa.standard.Scheme", "isEven");
      defProcStFld("max", "kawa.lib.numbers");
      defProcStFld("min", "kawa.lib.numbers");
      defProcStFld("+", "gnu.kawa.functions.AddOp", "$Pl");
      defProcStFld("-", "gnu.kawa.functions.AddOp", "$Mn");
      defProcStFld("*", "gnu.kawa.functions.MultiplyOp", "$St");
      defProcStFld("/", "gnu.kawa.functions.DivideOp", "$Sl");
      defProcStFld("abs", "kawa.lib.numbers");
      defProcStFld("quotient", "gnu.kawa.functions.DivideOp", "quotient");
      defProcStFld("remainder", "gnu.kawa.functions.DivideOp", "remainder");
      defProcStFld("modulo", "gnu.kawa.functions.DivideOp", "modulo");
      defProcStFld("div", "gnu.kawa.functions.DivideOp", "div");
      defProcStFld("mod", "gnu.kawa.functions.DivideOp", "mod");
      defProcStFld("div0", "gnu.kawa.functions.DivideOp", "div0");
      defProcStFld("mod0", "gnu.kawa.functions.DivideOp", "mod0");
      defProcStFld("div-and-mod", "kawa.lib.numbers");
      defProcStFld("div0-and-mod0", "kawa.lib.numbers");
      defProcStFld("gcd", "kawa.lib.numbers");
      defProcStFld("lcm", "kawa.lib.numbers");
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
      defProcStFld("atan", "kawa.lib.numbers");
      defProcStFld("sqrt", "kawa.lib.numbers");
      defProcStFld("expt", "kawa.standard.expt");
      defProcStFld("make-rectangular", "kawa.lib.numbers");
      defProcStFld("make-polar", "kawa.lib.numbers");
      defProcStFld("real-part", "kawa.lib.numbers");
      defProcStFld("imag-part", "kawa.lib.numbers");
      defProcStFld("magnitude", "kawa.lib.numbers");
      defProcStFld("angle", "kawa.lib.numbers");
      defProcStFld("inexact", "kawa.lib.numbers");
      defProcStFld("exact", "kawa.lib.numbers");
      defProcStFld("exact->inexact", "kawa.lib.numbers");
      defProcStFld("inexact->exact", "kawa.lib.numbers");
      defProcStFld("number->string", "kawa.lib.numbers");
      defProcStFld("string->number", "kawa.lib.numbers");

      //-- Section 6.6  -- complete
      defProcStFld("char?", "kawa.lib.characters");
      defProcStFld("char=?", "kawa.lib.characters");
      defProcStFld("char<?", "kawa.lib.characters");
      defProcStFld("char>?", "kawa.lib.characters");
      defProcStFld("char<=?", "kawa.lib.characters");
      defProcStFld("char>=?", "kawa.lib.characters");
      defProcStFld("char-ci=?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-ci<?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-ci>?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-ci<=?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-ci>=?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-alphabetic?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-numeric?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-whitespace?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-upper-case?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-lower-case?", "kawa.lib.rnrs.unicode");
      defProcStFld("char-title-case?", "kawa.lib.rnrs.unicode");
      defProcStFld("char->integer", "kawa.lib.characters");
      defProcStFld("integer->char", "kawa.lib.characters");
      defProcStFld("char-upcase", "kawa.lib.rnrs.unicode");
      defProcStFld("char-downcase", "kawa.lib.rnrs.unicode");
      defProcStFld("char-titlecase", "kawa.lib.rnrs.unicode");
      defProcStFld("char-foldcase", "kawa.lib.rnrs.unicode");
      defProcStFld("char-general-category", "kawa.lib.rnrs.unicode");
      
      //-- Section 6.7  -- complete
      defProcStFld("string?", "kawa.lib.strings");
      defProcStFld("make-string", "kawa.lib.strings");
      defProcStFld("string-length", "kawa.lib.strings");
      defProcStFld("string-ref", "kawa.lib.strings");
      defProcStFld("string-set!", "kawa.lib.strings");

      defProcStFld("string=?", "kawa.lib.strings");
      defProcStFld("string<?", "kawa.lib.strings");
      defProcStFld("string>?", "kawa.lib.strings");
      defProcStFld("string<=?", "kawa.lib.strings");
      defProcStFld("string>=?", "kawa.lib.strings");

      defProcStFld("string-ci=?", "kawa.lib.rnrs.unicode");
      defProcStFld("string-ci<?", "kawa.lib.rnrs.unicode");
      defProcStFld("string-ci>?", "kawa.lib.rnrs.unicode");
      defProcStFld("string-ci<=?", "kawa.lib.rnrs.unicode");
      defProcStFld("string-ci>=?", "kawa.lib.rnrs.unicode");
      defProcStFld("string-normalize-nfd", "kawa.lib.rnrs.unicode");
      defProcStFld("string-normalize-nfkd", "kawa.lib.rnrs.unicode");
      defProcStFld("string-normalize-nfc", "kawa.lib.rnrs.unicode");
      defProcStFld("string-normalize-nfkc", "kawa.lib.rnrs.unicode");

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
      defProcStFld("apply", "kawa.standard.Scheme", "apply");
      defProcStFld("map", "kawa.standard.Scheme", "map");
      defProcStFld("for-each", "kawa.standard.Scheme", "forEach");
      defProcStFld("call-with-current-continuation",
                   "gnu.kawa.functions.CallCC", "callcc");
      defProcStFld("call/cc", "kawa.standard.callcc", "callcc");
      defProcStFld("force", "kawa.lib.misc");

      //-- Section 6.10  -- complete
      defProcStFld("call-with-input-file", "kawa.lib.ports");
      defProcStFld("call-with-output-file", "kawa.lib.ports");
      defProcStFld("input-port?", "kawa.lib.ports");
      defProcStFld("output-port?", "kawa.lib.ports");
      defProcStFld("current-input-port", "kawa.lib.ports");
      defProcStFld("current-output-port", "kawa.lib.ports");
      defProcStFld("with-input-from-file", "kawa.lib.ports");
      defProcStFld("with-output-to-file", "kawa.lib.ports");
      defProcStFld("open-input-file", "kawa.lib.ports");
      defProcStFld("open-output-file", "kawa.lib.ports");
      defProcStFld("close-input-port", "kawa.lib.ports");
      defProcStFld("close-output-port", "kawa.lib.ports");
      defProcStFld("read", "kawa.lib.ports");
      defProcStFld("read-line", "kawa.lib.ports");
      defProcStFld("read-char", "kawa.standard.readchar", "readChar");
      defProcStFld("peek-char", "kawa.standard.readchar", "peekChar");
      defProcStFld("eof-object?", "kawa.lib.ports");
      defProcStFld("char-ready?", "kawa.lib.ports");
      defProcStFld("write", "kawa.lib.ports");
      defProcStFld("display", "kawa.lib.ports");
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

      defProcStFld("%syntax-error", "kawa.standard.syntax_error",
                   "syntax_error");
      defProcStFld("syntax-error", "kawa.lib.prim_syntax");

      r4Environment.setLocked();
      environ = r5Environment;

      defProcStFld("values", "kawa.lib.misc");
      defProcStFld("call-with-values", "kawa.standard.call_with_values",
		   "callWithValues");
      defSntxStFld("let-values", "kawa.lib.syntax");
      defSntxStFld("let*-values", "kawa.lib.syntax");
      defSntxStFld("case-lambda", "kawa.lib.syntax");
      defSntxStFld("receive", "kawa.lib.syntax");
      defProcStFld("eval", "kawa.lang.Eval");
      defProcStFld("repl", "kawa.standard.SchemeCompilation", "repl");
      defProcStFld("scheme-report-environment", "kawa.lib.misc");
      defProcStFld("null-environment", "kawa.lib.misc");
      defProcStFld("interaction-environment", "kawa.lib.misc");
      defProcStFld("dynamic-wind", "kawa.lib.misc");

      r5Environment.setLocked();
      environ = kawaEnvironment;
 
      defSntxStFld("define-private", "kawa.lib.prim_syntax");
      defSntxStFld("define-constant", "kawa.lib.prim_syntax");

      defSntxStFld("define-autoload",
                   "kawa.standard.define_autoload", "define_autoload");
      defSntxStFld("define-autoloads-from-file",
                   "kawa.standard.define_autoload",
                   "define_autoloads_from_file");

      defProcStFld("exit", "kawa.lib.rnrs.programs");
      defProcStFld("command-line", "kawa.lib.rnrs.programs");

      defProcStFld("bitwise-arithmetic-shift",
                   "gnu.kawa.functions.BitwiseOp", "ashift");
      defProcStFld("arithmetic-shift",
                   "gnu.kawa.functions.BitwiseOp", "ashift");
      defProcStFld("ash",
                   "gnu.kawa.functions.BitwiseOp", "ashift");
      defProcStFld("bitwise-arithmetic-shift-left",
                   "gnu.kawa.functions.BitwiseOp", "ashiftl");
      defProcStFld("bitwise-arithmetic-shift-right",
                   "gnu.kawa.functions.BitwiseOp", "ashiftr");
      defProcStFld("bitwise-and", "gnu.kawa.functions.BitwiseOp", "and");
      defProcStFld("logand", "gnu.kawa.functions.BitwiseOp", "and");
      defProcStFld("bitwise-ior", "gnu.kawa.functions.BitwiseOp", "ior");
      defProcStFld("logior", "gnu.kawa.functions.BitwiseOp", "ior");
      defProcStFld("bitwise-xor", "gnu.kawa.functions.BitwiseOp", "xor");
      defProcStFld("logxor", "gnu.kawa.functions.BitwiseOp", "xor");
      defProcStFld("bitwise-if", "kawa.lib.numbers");
      defProcStFld("bitwise-not", "gnu.kawa.functions.BitwiseOp", "not");
      defProcStFld("lognot", "gnu.kawa.functions.BitwiseOp", "not");
      defProcStFld("logop", "kawa.lib.numbers");
      defProcStFld("bitwise-bit-set?", "kawa.lib.numbers");
      defProcStFld("logbit?", "kawa.lib.numbers",
                   Compilation.mangleNameIfNeeded("bitwise-bit-set?"));
      defProcStFld("logtest", "kawa.lib.numbers");
      defProcStFld("bitwise-bit-count", "kawa.lib.numbers");
      defProcStFld("logcount", "kawa.lib.numbers");
      defProcStFld("bitwise-copy-bit", "kawa.lib.numbers");
      defProcStFld("bitwise-copy-bit-field", "kawa.lib.numbers");
      defProcStFld("bitwise-bit-field", "kawa.lib.numbers");
      defProcStFld("bit-extract", "kawa.lib.numbers",
                   Compilation.mangleNameIfNeeded("bitwise-bit-field"));
      defProcStFld("bitwise-length", "kawa.lib.numbers");
      defProcStFld("integer-length", "kawa.lib.numbers", "bitwise$Mnlength");
      defProcStFld("bitwise-first-bit-set", "kawa.lib.numbers");
      defProcStFld("bitwise-rotate-bit-field", "kawa.lib.numbers");
      defProcStFld("bitwise-reverse-bit-field", "kawa.lib.numbers");

      // These are from SLIB.
      defProcStFld("string-upcase!", "kawa.lib.strings");
      defProcStFld("string-downcase!", "kawa.lib.strings");
      defProcStFld("string-capitalize!", "kawa.lib.strings");
      defProcStFld("string-upcase", "kawa.lib.rnrs.unicode");
      defProcStFld("string-downcase", "kawa.lib.rnrs.unicode");
      defProcStFld("string-titlecase", "kawa.lib.rnrs.unicode");
      defProcStFld("string-foldcase", "kawa.lib.rnrs.unicode");
      defProcStFld("string-capitalize", "kawa.lib.strings");
      defSntxStFld("primitive-virtual-method",
                   "kawa.standard.prim_method", "virtual_method");
      defSntxStFld("primitive-static-method",
                   "kawa.standard.prim_method", "static_method");
      defSntxStFld("primitive-interface-method",
                   "kawa.standard.prim_method", "interface_method");
      defSntxStFld("primitive-constructor", "kawa.lib.reflection");
      defSntxStFld("primitive-op1",
                   "kawa.standard.prim_method", "op1");
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
      defProcStFld("catch", "kawa.lib.system");
      defProcStFld("error", "kawa.lib.misc");
      defProcStFld("as", "gnu.kawa.functions.Convert", "as");
      defProcStFld("instance?", "kawa.standard.Scheme", "instanceOf");
      defSntxStFld("synchronized", "kawa.lib.syntax");
      defSntxStFld("object", "kawa.standard.object", "objectSyntax");
      defSntxStFld("define-class", "kawa.standard.define_class",
                   "define_class");
      defSntxStFld("define-simple-class", "kawa.standard.define_class",
                   "define_simple_class");
      defSntxStFld("this", "kawa.standard.thisRef", "thisSyntax");
      defProcStFld("make", "gnu.kawa.reflect.Invoke", "make");
      defProcStFld("slot-ref", "gnu.kawa.reflect.SlotGet", "slotRef");
      defProcStFld("slot-set!", "gnu.kawa.reflect.SlotSet", "set$Mnfield$Ex");
      defProcStFld("field", "gnu.kawa.reflect.SlotGet");
      defProcStFld("class-methods", "gnu.kawa.reflect.ClassMethods",
		   "classMethods");
      defProcStFld("static-field", "gnu.kawa.reflect.SlotGet",
		   "staticField");
      defProcStFld("invoke", "gnu.kawa.reflect.Invoke", "invoke");

      defProcStFld("invoke-static", "gnu.kawa.reflect.Invoke", "invokeStatic");
      defProcStFld("invoke-special", "gnu.kawa.reflect.Invoke", "invokeSpecial");

      defSntxStFld("define-macro", "kawa.lib.syntax");
      defSntxStFld("%define-macro", "kawa.standard.define_syntax",
                   "define_macro");
      defSntxStFld("define-syntax-case", "kawa.lib.syntax");
      defSntxStFld("syntax-case", "kawa.standard.syntax_case", "syntax_case");
      defSntxStFld("%define-syntax", "kawa.standard.define_syntax",
                   "define_syntax");
      defSntxStFld("syntax", "kawa.standard.syntax", "syntax");
      defSntxStFld("quasisyntax", "kawa.standard.syntax", "quasiSyntax");
      defProcStFld("syntax-object->datum", "kawa.lib.std_syntax");
      defProcStFld("datum->syntax-object", "kawa.lib.std_syntax");
      defProcStFld("syntax->expression", "kawa.lib.prim_syntax");
      defProcStFld("syntax-body->expression", "kawa.lib.prim_syntax");
      defProcStFld("generate-temporaries", "kawa.lib.std_syntax");
      defSntxStFld("with-syntax", "kawa.lib.std_syntax");
      defProcStFld("identifier?", "kawa.lib.std_syntax");
      defProcStFld("free-identifier=?", "kawa.lib.std_syntax");
      defProcStFld("syntax-source", "kawa.lib.std_syntax");
      defProcStFld("syntax-line", "kawa.lib.std_syntax");
      defProcStFld("syntax-column", "kawa.lib.std_syntax");
      defSntxStFld("begin-for-syntax", "kawa.lib.std_syntax");
      defSntxStFld("define-for-syntax", "kawa.lib.std_syntax");
      defSntxStFld("include", "kawa.lib.misc_syntax");
      defSntxStFld("include-relative", "kawa.lib.misc_syntax");

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
      defProcStFld("tokenize-string-to-string-array", "kawa.lib.system");
      defProcStFld("tokenize-string-using-shell", "kawa.lib.system");
      defProcStFld("command-parse", "kawa.lib.system");
      defProcStFld("process-command-line-assignments", "kawa.lib.system");
      
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
      defSntxStFld("fluid-let", "kawa.standard.fluid_let", "fluid_let");
      defSntxStFld("constant-fold", "kawa.standard.constant_fold",
                   "constant_fold");
      defProcStFld("make-parameter", "kawa.lib.parameters");
      defSntxStFld("parameterize", "kawa.lib.parameters");

      defProcStFld("compile-file", "kawa.lib.system");
      defProcStFld("environment-bound?", "kawa.lib.misc");
      defProcStFld("scheme-implementation-version", "kawa.lib.misc");
      defProcStFld("scheme-window", "kawa.lib.windows");
      defSntxStFld("define-procedure", "kawa.lib.std_syntax");
      defProcStFld("add-procedure-properties", "kawa.lib.misc");
      defProcStFld("make-procedure",
                   "gnu.kawa.functions.MakeProcedure", "makeProcedure");
      defProcStFld("procedure-property", "kawa.lib.misc");
      defProcStFld("set-procedure-property!", "kawa.lib.misc");
      defSntxStFld("provide", "kawa.lib.misc_syntax");
      defSntxStFld("test-begin", "kawa.lib.misc_syntax");

      defProcStFld("quantity->number", "kawa.lib.numbers");
      defProcStFld("quantity->unit", "kawa.lib.numbers");
      defProcStFld("make-quantity", "kawa.lib.numbers");
      defSntxStFld("define-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_namespace");
      defSntxStFld("define-xml-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_xml_namespace");
      defSntxStFld("define-private-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_private_namespace");
      defSntxStFld("define-unit", "kawa.standard.define_unit", "define_unit");
      defSntxStFld("define-base-unit", "kawa.standard.define_unit",
                   "define_base_unit");
      defProcStFld("duration", "kawa.lib.numbers");

      defProcStFld("gentemp", "kawa.lib.misc");
      defSntxStFld("defmacro", "kawa.lib.syntax");
      defProcStFld("setter", "gnu.kawa.functions.Setter", "setter");

      defSntxStFld("resource-url", "kawa.lib.misc_syntax");
      defSntxStFld("module-uri", "kawa.lib.misc_syntax");

      defSntxStFld("future", "kawa.lib.thread");
      defProcStFld("sleep", "kawa.lib.thread");
      defProcStFld("runnable", "kawa.lib.thread");

      defSntxStFld("trace", "kawa.lib.trace");
      defSntxStFld("untrace", "kawa.lib.trace");
      defSntxStFld("disassemble", "kawa.lib.trace");

      defProcStFld("format", "gnu.kawa.functions.Format");
      defProcStFld("parse-format", "gnu.kawa.functions.ParseFormat", "parseFormat");

      defProcStFld("make-element", "gnu.kawa.xml.MakeElement", "makeElement");
      defProcStFld("make-attribute", "gnu.kawa.xml.MakeAttribute", "makeAttribute");
      defProcStFld("map-values", "gnu.kawa.functions.ValuesMap", "valuesMap");
      defProcStFld("children", "gnu.kawa.xml.Children", "children");
      defProcStFld("attributes", "gnu.kawa.xml.Attributes");
      defProcStFld("unescaped-data", "gnu.kawa.xml.MakeUnescapedData",
		   "unescapedData");
      defProcStFld("keyword?", "kawa.lib.keywords");
      defProcStFld("keyword->string", "kawa.lib.keywords");
      defProcStFld("string->keyword", "kawa.lib.keywords");
      defSntxStFld("location", "kawa.standard.location", "location");
      defSntxStFld("define-alias", "kawa.standard.define_alias",
                   "define_alias");
      defSntxStFld("define-variable", "kawa.standard.define_variable",
                   "define_variable");
      defSntxStFld("define-member-alias", "kawa.standard.define_member_alias",
                   "define_member_alias");
      defSntxStFld("define-enum", "gnu.kawa.slib.enums");
      defSntxStFld("import", "kawa.lib.syntax");
      defSntxStFld("require", "kawa.standard.require", "require");
      defSntxStFld("module-name", "kawa.standard.module_name",
                   "module_name");
      defSntxStFld("module-extends", "kawa.standard.module_extends",
                   "module_extends");
      defSntxStFld("module-implements", "kawa.standard.module_implements",
                   "module_implements");
      defSntxStFld("module-static", "kawa.standard.module_static",
                   "module_static");
      defSntxStFld("module-export", "kawa.standard.export", "module_export");
      defSntxStFld("export", "kawa.standard.export", "export");
      defSntxStFld("module-compile-options",
                   "kawa.standard.module_compile_options",
                   "module_compile_options");
      defSntxStFld("with-compile-options",
                   "kawa.standard.with_compile_options",
                   "with_compile_options");

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

      defSntxStFld("cond-expand", "kawa.lib.syntax");
      defSntxStFld("%cond-expand", "kawa.lib.syntax");

      defAliasStFld("*print-base*", "gnu.kawa.functions.DisplayFormat",
                    "outBase");
      defAliasStFld("*print-radix*", "gnu.kawa.functions.DisplayFormat",
                    "outRadix");
      defAliasStFld("*print-right-margin*",
                    "gnu.text.PrettyWriter", "lineLengthLoc");
      defAliasStFld("*print-miser-width*",
                    "gnu.text.PrettyWriter", "miserWidthLoc");
      defAliasStFld("html", "gnu.kawa.xml.XmlNamespace", "HTML");
      defAliasStFld("unit", "kawa.standard.Scheme", "unitNamespace");

      defAliasStFld("path", "gnu.kawa.lispexpr.LangObjType", "pathType");
      defAliasStFld("filepath", "gnu.kawa.lispexpr.LangObjType", "filepathType");
      defAliasStFld("URI", "gnu.kawa.lispexpr.LangObjType", "URIType");
      defProcStFld("resolve-uri", "kawa.lib.files");

      defAliasStFld("vector", "gnu.kawa.lispexpr.LangObjType", "vectorType");
      defAliasStFld("string", "gnu.kawa.lispexpr.LangObjType", "stringType");
      defAliasStFld("list", "gnu.kawa.lispexpr.LangObjType", "listType");
      defAliasStFld("regex", "gnu.kawa.lispexpr.LangObjType", "regexType");
      defProcStFld("path?", "kawa.lib.files");
      defProcStFld("filepath?", "kawa.lib.files");
      defProcStFld("URI?", "kawa.lib.files");
      defProcStFld("absolute-path?", "kawa.lib.files");
      defProcStFld("path-scheme", "kawa.lib.files");
      defProcStFld("path-authority", "kawa.lib.files");
      defProcStFld("path-user-info", "kawa.lib.files");
      defProcStFld("path-host", "kawa.lib.files");
      defProcStFld("path-port", "kawa.lib.files");
      defProcStFld("path-file", "kawa.lib.files");
      defProcStFld("path-parent", "kawa.lib.files");
      defProcStFld("path-directory", "kawa.lib.files");
      defProcStFld("path-last", "kawa.lib.files");
      defProcStFld("path-extension", "kawa.lib.files");
      defProcStFld("path-fragment", "kawa.lib.files");
      defProcStFld("path-query", "kawa.lib.files");

      kawaEnvironment.setLocked();
  }

  public Scheme ()
  {
    environ = kawaEnvironment;
    userEnv = getNewEnvironment();
  }

  protected Scheme (Environment env)
  {
    environ = env;
  }

  public String getName()
  {
    return "Scheme";
  }

  /** Evalutate Scheme expressions from string.
   * @param string the string constaining Scheme expressions
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (String string, Environment env)
  {
    return eval (new CharArrayInPort(string), env);
  }

  /** Evalutate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (InPort port, Environment env)
  {
    SourceMessages messages = new SourceMessages();
    try
      {
	LispReader lexer = (LispReader)
	  Language.getDefaultLanguage().getLexer(port, messages);
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

  public static final AbstractFormat writeFormat = new DisplayFormat(true, 'S');
  public static final AbstractFormat displayFormat = new DisplayFormat(false, 'S');
  public AbstractFormat getFormat(boolean readable)
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

  static HashMap<String,Type> types;
  static HashMap<Type,String> typeToStringMap;

  static synchronized HashMap<String,Type> getTypeMap ()
  {
    if (types == null)
      {
	booleanType
	  = new LangPrimType(Type.booleanType, Scheme.getInstance());
	types = new HashMap<String,Type> ();
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

	types.put ("Object", Type.objectType);
	types.put ("String", Type.toStringType);

	types.put ("object", Type.objectType);
	types.put ("number", LangObjType.numericType);
	types.put ("quantity", ClassType.make("gnu.math.Quantity"));
	types.put ("complex", ClassType.make("gnu.math.Complex"));
	types.put ("real", LangObjType.realType);
	types.put ("rational", LangObjType.rationalType);
	types.put ("integer", LangObjType.integerType);
	types.put ("symbol", ClassType.make("gnu.mapping.Symbol"));
	types.put ("namespace", ClassType.make("gnu.mapping.Namespace"));
	types.put ("keyword", ClassType.make("gnu.expr.Keyword"));
	types.put ("pair", ClassType.make("gnu.lists.Pair"));
	types.put ("pair-with-position",
		   ClassType.make("gnu.lists.PairWithPosition"));
	types.put ("constant-string", ClassType.make("java.lang.String"));
	types.put ("abstract-string", ClassType.make("gnu.lists.CharSeq"));
	types.put ("character", ClassType.make("gnu.text.Char"));
	types.put ("vector", LangObjType.vectorType);
	types.put ("string", LangObjType.stringType);
	types.put ("list", LangObjType.listType);
	types.put ("function", ClassType.make("gnu.mapping.Procedure"));
	types.put ("procedure", ClassType.make("gnu.mapping.Procedure"));
	types.put ("input-port", ClassType.make("gnu.mapping.InPort"));
	types.put ("output-port", ClassType.make("gnu.mapping.OutPort"));
	types.put ("string-output-port",
                   ClassType.make("gnu.mapping.CharArrayOutPort"));
	types.put ("record", ClassType.make("kawa.lang.Record"));
	types.put ("type", LangObjType.typeType);
	types.put ("class-type", LangObjType.typeClassType);
	types.put ("class", LangObjType.typeClass);

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
        types.put ("document", ClassType.make("gnu.kawa.xml.KDocument"));
        types.put ("readtable", ClassType.make("gnu.kawa.lispexpr.ReadTable"));
      }
    return types;
  }

  public static Type getNamedType (String name)
  {
    getTypeMap();
    Type type = (Type) types.get(name);
    if (type == null
	&& (name.startsWith("elisp:") || name.startsWith("clisp:")))
      {
	int colon = name.indexOf(':');
	Class clas = getNamedType(name.substring(colon+1)).getReflectClass();
	String lang = name.substring(0,colon);
	Language interp = Language.getInstance(lang);
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
      return Type.toStringType;
    if ("gnu.math.IntNum".equals(name))
      return LangObjType.integerType;
    if ("gnu.math.DFloNum".equals(name))
      return LangObjType.dflonumType;
    if ("gnu.math.RatNum".equals(name))
      return LangObjType.rationalType;
    if ("gnu.math.RealNum".equals(name))
      return LangObjType.realType;
    if ("gnu.math.Numeric".equals(name))
      return LangObjType.numericType;
    if ("gnu.lists.FVector".equals(name))
      return LangObjType.vectorType;
    if ("gnu.lists.LList".equals(name))
      return LangObjType.listType;
    if ("gnu.text.Path".equals(name))
      return LangObjType.pathType;
    if ("gnu.text.URIPath".equals(name))
      return LangObjType.URIType;
    if ("gnu.text.FilePath".equals(name))
      return LangObjType.filepathType;
    if ("java.lang.Class".equals(name))
      return LangObjType.typeClass;
    if ("gnu.bytecode.Type".equals(name))
      return LangObjType.typeType;
    if ("gnu.bytecode.ClassType".equals(name))
      return LangObjType.typeClassType;
    return Type.make(clas);
  }

  public String formatType (Type type)
  {
    // FIXME synchronize
    if (typeToStringMap == null)
      {
        typeToStringMap = new HashMap<Type,String>();
        for (java.util.Map.Entry<String,Type> e : getTypeMap().entrySet())
          {
            String s = e.getKey();
            Type t = e.getValue();
            typeToStringMap.put(t, s);
            Type it = t.getImplementationType();
            if (it != t)
              typeToStringMap.put(it, s);
          }
      }
    String str = typeToStringMap.get(type);
    if (str != null)
      return str;
    return super.formatType(type);
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
    t = Language.string2Type(name);
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

  public static final Namespace unitNamespace =
    Namespace.valueOf("http://kawa.gnu.org/unit", "unit");

  /** If a symbol is lexically unbound, look for a default binding.
   * If the symbol is the name of an existing Java type/class,
   * return that Class.
   * Handles both with and without (semi-deprecated) angle-brackets:
   * {@code <java.lang.Integer>} and {@code java.lang.Integer}.
   * Also handles arrays, such as {@code java.lang.String[]}.
   */
  public Expression checkDefaultBinding (Symbol symbol, Translator tr)
  {
    Namespace namespace = symbol.getNamespace();
    String local = symbol.getLocalPart();
    if (namespace instanceof XmlNamespace)
      return QuoteExp.getInstance(((XmlNamespace) namespace).get(local));
    if (namespace.getName() == unitNamespace.getName())
      {
        Object val = Unit.lookup(local);
        if (val != null)
          return QuoteExp.getInstance(val);
      }
    String name = symbol.toString();
    int len = name.length();
    if (len == 0)
      return null;
    if (len > 1 && name.charAt(len-1) == '?')
      {
        int llen = local.length();
        if (llen > 1)
          {
            String tlocal = local.substring(0, llen-1).intern();
            Symbol tsymbol = namespace.getSymbol(tlocal);
            Expression texp = tr.rewrite(tsymbol, false);
            if (texp instanceof ReferenceExp)
              {
                Declaration decl = ((ReferenceExp) texp).getBinding();
                if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
                  texp = null;
              }
            else if (! (texp instanceof QuoteExp))
              texp = null;
            if (texp != null)
              {
                LambdaExp lexp = new LambdaExp(1);
                lexp.setSymbol(symbol);
                Declaration param = lexp.addDeclaration((Object) null);
                lexp.body = new ApplyExp(instanceOf,
                                         new Expression[] {
                                           new ReferenceExp(param), texp, });
                return lexp;
              }
          }
      }
    char ch0 = name.charAt(0);

    // Look for quantities.
    if (ch0 == '-' || ch0 == '+' || Character.digit(ch0, 10) >= 0)
      {
        // 1: initial + or -1 seen.
        // 2: digits seen
        // 3: '.' seen
        // 4: fraction seen
        // 5: [eE][=+]?[0-9]+ seen
        int state = 0;
        int i = 0;
        for (;  i < len;  i++)
          {
            char ch = name.charAt(i);
            if (Character.digit(ch, 10) >= 0)
              state = state < 3 ? 2 : state < 5 ? 4 : 5;
            else if ((ch == '+' || ch == '-') && state == 0)
              state = 1;
            else if (ch == '.' && state < 3)
              state = 3;
            else if ((ch == 'e' || ch == 'E') && (state == 2 || state == 4)
                     && i + 1 < len)
              {
                int j = i+1;
                char next = name.charAt(j);
                if ((next == '-' || next == '+') && ++j < len)
                  next = name.charAt(j);
                if (Character.digit(next, 10) < 0)
                  break;
                state = 5;
                i = j+1;
              }
            else
              break;
          }
      tryQuantity:
        if (i < len && state > 1)
          {
            DFloNum num = new DFloNum(name.substring(0,i));
            boolean div = false;
            Vector vec = new Vector();
            for (; i < len ;)
              {
                char ch = name.charAt(i++);
                if (ch == '*')
                  {
                    if (i == len) break tryQuantity;
                    ch = name.charAt(i++);
                  }
                else if (ch == '/')
                  {
                    if (i == len || div) break tryQuantity; 
                    div = true;
                    ch = name.charAt(i++);
                  }
                int unitStart = i-1;
                int unitEnd;
                for (;;)
                  {
                    if (! Character.isLetter(ch))
                      {
                        unitEnd = i - 1;
                        if (unitEnd == unitStart)
                          break tryQuantity;
                        break;
                      }
                    if (i == len)
                      {
                        unitEnd = i;
                        ch = '1';
                        break;
                      }
                    ch = name.charAt(i++);
                  }
                vec.addElement(name.substring(unitStart, unitEnd));
                boolean expRequired = false;
                if (ch == '^')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                  }
                boolean neg = div;
                if (ch == '+')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                  }
                else if (ch == '-')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                    neg = ! neg;
                  }
                int nexp = 0;
                int exp = 0;
                for (;;)
                  {
                    int dig = Character.digit(ch, 10);
                    if (dig <= 0)
                      {
                        i--;
                        break;
                      }
                    exp = 10 * exp + dig;
                    nexp++;
                    if (i == len)
                      break;
                    ch = name.charAt(i++);
                  }
                if (nexp == 0)
                  {
                    exp = 1;
                    if (expRequired)
                      break tryQuantity;
                  }
                if (neg)
                  exp = - exp;
                vec.addElement(IntNum.make(exp));
              }
            if (i == len)
              {
                int nunits = vec.size() >> 1;
                Expression[] units = new Expression[nunits];
                for (i = 0;  i < nunits;  i++)
                  {
                    String uname = (String) vec.elementAt(2*i);
                    Symbol usym = unitNamespace.getSymbol(uname.intern());
                    Expression uref = tr.rewrite(usym);
                    IntNum uexp = (IntNum) vec.elementAt(2*i+1);
                    if (uexp.longValue() != 1)
                      uref = new ApplyExp(expt.expt,
                                          new Expression[] { uref , QuoteExp.getInstance(uexp) });
                    units[i] = uref;
                  }
                Expression unit;
                if (nunits == 1)
                  unit = units[0];
                else
                  unit = new ApplyExp(MultiplyOp.$St, units);
                return new ApplyExp(MultiplyOp.$St,
                                    new Expression[] { QuoteExp.getInstance(num),
                                                       unit });
              }
          }
      }

    boolean sawAngle;
    if (len > 2 && ch0 == '<' && name.charAt(len-1) == '>')
      {
        name = name.substring(1, len-1);
        len -= 2;
        sawAngle = true;
      }
    else
      sawAngle = false;
    int rank = 0;
    while (len > 2 && name.charAt(len-2) == '[' && name.charAt(len-1) == ']')
      {
        len -= 2;
        rank++;
      }

    String cname = name;
    if (rank != 0)
      cname = name.substring(0, len);
    try
      { 
        Class clas;
        Type type = getNamedType(cname);
        if (rank > 0 && (! sawAngle || type == null))
          {
            Symbol tsymbol = namespace.getSymbol(cname.intern());
            Expression texp = tr.rewrite(tsymbol, false);
            texp = InlineCalls.inlineCalls(texp, tr);
            if (! (texp instanceof ErrorExp))
              type = tr.getLanguage().getTypeFor(texp);
          }
        if (type != null)
          {
            // Somewhat inconsistent: Types named by getNamedType are Type,
            // while standard type/classes are Class.  FIXME.
            while (--rank >= 0)
              type = gnu.bytecode.ArrayType.make(type);
            return QuoteExp.getInstance(type);
          }
        else
          {
            type = Type.lookupType(cname);
            if (type instanceof gnu.bytecode.PrimType)
              clas = type.getReflectClass();
            else
              {
                if (cname.indexOf('.') < 0)
                  cname = (tr.classPrefix
                           + Compilation.mangleNameIfNeeded(cname));
                clas = ClassType.getContextClass(cname);
              }
          }
        if (clas != null)
          {
            if (rank > 0)
              {
                type = Type.make(clas);
                while (--rank >= 0)
                  type = gnu.bytecode.ArrayType.make(type);
                clas = type.getReflectClass();
              }
            return QuoteExp.getInstance(clas);
          }
      }
    catch (ClassNotFoundException ex)
      {
        Package pack = gnu.bytecode.ArrayClassLoader.getContextPackage(name);
        if (pack != null)
          return QuoteExp.getInstance(pack);
      }
    catch (Throwable ex)
      {
      }
    return null;
  }

  public Expression makeApply (Expression func, Expression[] args)
  {
    Expression[] exps = new Expression[args.length+1];
    exps[0] = func;
    System.arraycopy(args, 0, exps, 1, args.length);
    return new ApplyExp(new ReferenceExp(applyFieldDecl), exps);
  }

  public Symbol asSymbol (String ident)
  {
    return Namespace.EmptyNamespace.getSymbol(ident);
  }

  public ReadTable createReadTable ()
  {
    ReadTable tab = ReadTable.createInitial();
    tab.postfixLookupOperator = ':';
    ReaderDispatch dispatchTable = (ReaderDispatch) tab.lookup('#');
    dispatchTable.set('\'', new ReaderQuote(asSymbol("syntax")));
    dispatchTable.set('`', new ReaderQuote(asSymbol("quasisyntax")));
    dispatchTable.set(',', ReaderDispatchMisc.getInstance());
    tab.putReaderCtorFld("path", "gnu.kawa.lispexpr.LangObjType", "pathType");
    tab.putReaderCtorFld("filepath", "gnu.kawa.lispexpr.LangObjType", "filepathType");
    tab.putReaderCtorFld("URI", "gnu.kawa.lispexpr.LangObjType", "URIType");
    tab.putReaderCtor("symbol", ClassType.make("gnu.mapping.Symbol"));
    tab.putReaderCtor("namespace", ClassType.make("gnu.mapping.Namespace"));
    tab.putReaderCtorFld("duration", "kawa.lib.numbers", "duration");
    tab.setFinalColonIsKeyword(true);
    return tab;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(getInstance());
  }
}
