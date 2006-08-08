// Copyright (c) 1999, 2000-2005, 2006 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.*;
import java.io.*;
import kawa.Shell;
import gnu.text.*;
import java.util.zip.*;
import java.util.Stack;

/** State for a single expression or module.
 * For each top-level thing (expression or file) we compile or evaluate
 * we create a new Compilation.
 */

public class Compilation
{
  /** True if the form is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a function (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile;

  /** Used by LambdaExp.getSelectorValue if need to allocate new selector. */
  int maxSelectorValue;

  public ClassType curClass;
  public ClassType mainClass;
  /** Generated class that extends ModuleBody.  Normally same as mainClass. */
  public ClassType moduleClass;

  public LambdaExp curLambda;
  public ModuleExp mainLambda;
  public Variable thisDecl;

  /** Contains "$instance" if the module is static; otherwise null. */
  Variable moduleInstanceVar;

  /** A code, one of the following constants, indicating how far along
   * we are in the parsing/compilation process.
   * These codes are even integers for completed stages and odd integers
   * for begun but not completed stages. */
  private int state;
  /** Returns a code indicating how far along
   * we are in the parsing/compilation process. */
  public int getState () { return state; }
  public void setState (int state) { this.state = state; }
  /** State code for initial pre-parse looking for module name. */
  public static final int PROLOG_PARSING = 1;
  /** We have determined the module name and class, but not finished parsing. */
  public static final int PROLOG_PARSED = 2;
  /** State code indicating the entire module has been parsed. */
  public static final int BODY_PARSED = 4;
  /** State code for lexical bindings having been resolved. */ 
  public static final int RESOLVED = 6;
  /** State code when various inlining and optimization passes are done. */
  public static final int WALKED = 8;
  /** State code that various compile-only data has been determined. */
  public static final int COMPILE_SETUP = 10;
  /** State code indicating the bytecode has been generated. */
  public static final int COMPILED = 12;
  /** State code indicating that bytecode has been written to its target. */
  public static final int CLASS_WRITTEN = 14;
  public static final int ERROR_SEEN = 100;

  public ModuleInfo minfo;
  public Lexer lexer;

  /** Used to access the "main" instance.
   * This is used for two different purposes, which may be confusing:
   * <ul>
   * <li>
   * If we're compiling a static module, then {@code moduleInstanceMainField}
   * is a field in {@code mainClass} named {@code "$instance"} that
   * points to the single instance of the module.</li>
   * <li>
   * If {@code moduleClass!=mainClass} (typically because we've specified
   * {@code module-extends}) <em>and</em> the module is non-static then
   * {@code moduleInstanceMainField} is a field in {@code moduleClass}
   * named {@code "$main"} that points back to {@code mainClass}.</li></ul>
   */
  Field moduleInstanceMainField;

  protected java.util.Stack pendingImports;

  public void pushPendingImport (ModuleInfo info, ScopeExp defs)
  {
    if (pendingImports == null)
      pendingImports = new java.util.Stack();
    pendingImports.push(info);
    pendingImports.push(defs);
    Expression posExp = new ReferenceExp((Object) null);
    posExp.setLine(this);
    pendingImports.push(posExp);
  }

  /** If true, minimize the number of classes generated.
   * Do this even if it makes things a little slower. */
  public static boolean fewerClasses;

  /** If true, print out expressions after parsing and before optimizations. */
  public static boolean debugPrintExpr = false;

  /** If true, print out final expressions after optimizations etc. */
  public static boolean debugPrintFinalExpr;

  public static Options options = new Options();
  public Options currentOptions = new Options(options);
  static {
    options.add("warn-undefined-variable", Options.BOOLEAN_OPTION,
		"warn if no compiler-visible binding for a variable");
    options.add("warn-invoke-unknown-method", Options.BOOLEAN_OPTION,
		"warn if invoke calls an unknown method");
    options.add("warn-as-error", Options.BOOLEAN_OPTION,
		"Make all warnings into errors");
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key, boolean defaultValue)
  {
    return currentOptions.getBoolean(key, defaultValue);
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key)
  {
    return currentOptions.getBoolean(key);
  }

  /** The default calling convention.
   * One of the following CALL_WITHG_xxx values. */
  public static int defaultCallConvention;
  public static final int CALL_WITH_UNSPECIFIED = 0;
  /** Plain calling convention, using regular Java parameters and returns. */
  public static final int CALL_WITH_RETURN = 1;
  /** Function results are written to the current CallContext's Consumer. */
  public static final int CALL_WITH_CONSUMER = 2;
  /** Like CALL_WITH_CONSUMER, but handle full on-stack-growing tail-calls. */
  public static final int CALL_WITH_TAILCALLS = 3;
  /** Support for full continuations.  Not implemented. */
  public static final int CALL_WITH_CONTINUATIONS = 4;

  public boolean usingCPStyle()
  { return defaultCallConvention == CALL_WITH_CONTINUATIONS; }
  public boolean usingTailCalls()
  { return defaultCallConvention >= CALL_WITH_TAILCALLS; }

  /** If moduleStatic > 0, (module-static #t) is implied by default.
   * If moduleStatic == 2, <clinit> calls run.
   * If moduleStatic < 0, (module-static #f) is implied by default. */
  public static int moduleStatic = 0;

  ClassType[] classes;
  int numClasses;

  /** When immediate, the ClassLoader we will load the compiled
   * classes from. */
  ArrayClassLoader loader;

  /** True if the compiled result will be immediately loaded. */ 
  public boolean immediate;

  /** The current method. */
  public Method method;

  public final CodeAttr getCode() { return method.getCode(); }

  int method_counter;

  /* When multiple procedures are compiled into a single method,
     we use a switch to jump to the correct part of the method. */
  SwitchState fswitch;

  Field fswitchIndex;

  // Various standard classes
  static public ClassType typeObject = Type.pointer_type;
  static public ClassType scmBooleanType = ClassType.make("java.lang.Boolean");
  static public ClassType typeString = ClassType.make("java.lang.String");
  static public ClassType javaStringType = typeString;
  static public ClassType scmKeywordType = ClassType.make("gnu.expr.Keyword");
  static public ClassType scmSequenceType = ClassType.make("gnu.lists.Sequence");
  static public ClassType javaIntegerType = ClassType.make("java.lang.Integer");
  static public ClassType scmListType = ClassType.make("gnu.lists.LList");
  static public ClassType typePair = ClassType.make("gnu.lists.Pair");
  static public ClassType scmPairType = typePair;
  static public ClassType scmUndefinedType = ClassType.make("gnu.expr.Undefined");
  public static final ArrayType objArrayType = ArrayType.make(typeObject);
  static public ClassType scmNamedType = ClassType.make("gnu.mapping.Named");
  static public ClassType typeRunnable = ClassType.make("java.lang.Runnable");
  public static ClassType typeType = ClassType.make("gnu.bytecode.Type");
  public static ClassType typeObjectType
    = ClassType.make("gnu.bytecode.ObjectType", typeType);
  public static ClassType typeClass = Type.java_lang_Class_type;
  static public ClassType typeClassType = ClassType.make("gnu.bytecode.ClassType", typeObjectType);
  static public ClassType typeProcedure
    = ClassType.make("gnu.mapping.Procedure");
  static public ClassType typeLanguage
    = ClassType.make("gnu.expr.Language");
  static public ClassType typeEnvironment
    = ClassType.make("gnu.mapping.Environment");
  static public ClassType typeLocation
    = ClassType.make("gnu.mapping.Location");
  static public ClassType typeSymbol
    = ClassType.make("gnu.mapping.Symbol");
  static public final Method getSymbolValueMethod
    = typeLanguage.getDeclaredMethod("getSymbolValue", 1);
  static public final Method getSymbolProcedureMethod
    = typeLanguage.getDeclaredMethod("getSymbolProcedure", 1);
  static public final Method getLocationMethod
    = typeLocation.addMethod("get", Type.typeArray0,
			    Type.pointer_type, Access.PUBLIC);
  static public final Method getProcedureBindingMethod
    = typeSymbol.addMethod("getProcedure", Type.typeArray0,
			    typeProcedure, Access.PUBLIC);
  static public final Field trueConstant
    = scmBooleanType.getDeclaredField("TRUE"); 
  static public final Field falseConstant
    = scmBooleanType.getDeclaredField("FALSE");

  static final Method setNameMethod
    = typeProcedure.getDeclaredMethod("setName", 1);
  static Method makeListMethod;
  
  public static final Type[] int1Args = { Type.int_type };
  public static final Type[] string1Arg = { javaStringType };
  public static final Type[] sym1Arg = string1Arg;

  static public final Method getLocation1EnvironmentMethod
  = typeEnvironment.getDeclaredMethod("getLocation", 1);
  static public final Method getLocation2EnvironmentMethod;
  static {
    Type[] args = { typeSymbol, Type.pointer_type };
    getLocation2EnvironmentMethod
      = typeEnvironment.addMethod("getLocation", args,
				  typeLocation, Access.PUBLIC|Access.FINAL);
  }

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
  }

  public static Method getCurrentEnvironmentMethod
    = typeEnvironment.addMethod("getCurrent", Type.typeArray0,
				typeEnvironment,Access.PUBLIC|Access.STATIC);

  public static Type[] apply0args = Type.typeArray0;
  public static Type[] apply1args = { typeObject };
  public static Type[] apply2args = { typeObject, typeObject };
  public static Type[] applyNargs = { objArrayType };

  static Method checkArgCountMethod;

  public static Method apply0method = typeProcedure.addMethod
  ("apply0", apply0args, typeObject, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;

  static
  {
    apply1method = typeProcedure.addMethod ("apply1", apply1args,
						typeObject, Access.PUBLIC);
    apply2method = typeProcedure.addMethod ("apply2", apply2args,
						typeObject, Access.PUBLIC);
    Type[] apply3args = { typeObject, typeObject, typeObject };
    apply3method = typeProcedure.addMethod ("apply3", apply3args,
						typeObject, Access.PUBLIC);
    Type[] apply4args = { typeObject , typeObject, typeObject, typeObject};
    apply4method = typeProcedure.addMethod ("apply4", apply4args,
						typeObject, Access.PUBLIC);
    applyNmethod = typeProcedure.addMethod ("applyN", applyNargs,
						typeObject, Access.PUBLIC);
    Type[] args = new Type[2];
    args[0] = typeProcedure;
    args[1] = Type.int_type;
    checkArgCountMethod
      = typeProcedure.addMethod("checkArgCount", args, Type.void_type,
				   Access.PUBLIC|Access.STATIC);
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  public static ClassType typeProcedure0
    = ClassType.make("gnu.mapping.Procedure0", typeProcedure);
  public static ClassType typeProcedure1
    = ClassType.make("gnu.mapping.Procedure1", typeProcedure);
  public static ClassType typeProcedure2
    = ClassType.make("gnu.mapping.Procedure2", typeProcedure);
  public static ClassType typeProcedure3
    = ClassType.make("gnu.mapping.Procedure3", typeProcedure);
  public static ClassType typeProcedure4
    = ClassType.make("gnu.mapping.Procedure4", typeProcedure);
  public static ClassType typeProcedureN
    = ClassType.make("gnu.mapping.ProcedureN", typeProcedure);
  public static ClassType typeModuleBody
    = ClassType.make("gnu.expr.ModuleBody", typeProcedure0);
  public static ClassType typeModuleWithContext
    = ClassType.make("gnu.expr.ModuleWithContext", typeModuleBody);
  public static ClassType typeApplet = ClassType.make("java.applet.Applet");
  public static ClassType typeServlet = ClassType.make("gnu.kawa.servlet.KawaServlet");

  /* Classes, fields, and methods used wgen usingCPStyle". */
  public static ClassType typeCallContext
    = ClassType.make("gnu.mapping.CallContext");
  public static final ClassType typeConsumer
    = ClassType.make("gnu.lists.Consumer");
  public static Method getCallContextInstanceMethod
    = typeCallContext.getDeclaredMethod("getInstance", 0);
  public static ClassType typeValues
    = ClassType.make("gnu.mapping.Values");
  public static Field noArgsField
    = typeValues.getDeclaredField("noArgs");
  public static Field pcCallContextField
    = typeCallContext.getDeclaredField("pc");
  public static ClassType typeMethodProc
  = ClassType.make("gnu.mapping.MethodProc", typeProcedureN);
  public static ClassType typeModuleMethod
  = ClassType.make("gnu.expr.ModuleMethod", typeMethodProc);
  //  public static Field numArgsCallFrameField = typeCallFrame.getDeclaredField("numArgs");
  public static Field argsCallContextField
    = typeCallContext.getDeclaredField("values");
  public static Field procCallContextField
    = typeCallContext.getDeclaredField("proc");
  private static Type[] applyCpsArgs = { typeCallContext};
  public static Method applyCpsMethod
    = typeProcedure.addMethod("apply", applyCpsArgs, Type.void_type,
				 Access.PUBLIC);

  public static ClassType[] typeProcedureArray = {
    typeProcedure0, typeProcedure1, typeProcedure2, typeProcedure3,
    typeProcedure4 };

  /** Rembembers stuff to do in <clinit> of main class. */
  Initializer clinitChain;

  public static boolean generateMainDefault = false;
  /** True if we should generate a main(String[]) method. */
  public boolean generateMain = generateMainDefault;

  LitTable litTable;

  public static boolean generateAppletDefault = false;
  /** True if we should generate an Applet. */
  public boolean generateApplet = generateAppletDefault;

  public static boolean generateServletDefault = false;
  /** True if we should generate an Servlet. */
  public boolean generateServlet = generateServletDefault;

  public final ClassType getModuleType()
  {
    return (defaultCallConvention >= Compilation.CALL_WITH_CONSUMER
	    ? typeModuleWithContext
	    : typeModuleBody);
  }

  /** Emit code to "evaluate" a compile-time constant.
   * This is the normal external interface.
   * @param value the value to be compiled
   */
  public void compileConstant (Object value)
  {
    gnu.bytecode.CodeAttr code = getCode();
    if (value == null)
      code.emitPushNull();
    else if (value instanceof String && ! immediate)
      code.emitPushString((String) value);
    else
      code.emitGetStatic(compileConstantToField(value));
  }

  public Field compileConstantToField (Object value)
  {
    Literal literal = litTable.findLiteral(value);
    if (literal.field == null)
      literal.assign(litTable);
    return literal.field;
  }

  public static boolean inlineOk = true;

  public boolean inlineOk (Expression proc)
  {
    if (proc instanceof LambdaExp)
      {
	// The compiler gets confused if we turn off inlining for nested
	// procedures - and they can't be rebound anyway.
	if (! (((LambdaExp) proc).currentLambda() instanceof ModuleExp))
	  return true;
      }
    return inlineOk;
  }

  public boolean inlineOk (Procedure proc)
  {
    return inlineOk;
  }

  public void compileConstant (Object value, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (value instanceof Values
	&& (target instanceof ConsumerTarget
	    || target instanceof SeriesTarget))
      {
	Object[] values = ((Values) value).getValues();
	for (int i = 0;  i < values.length;  i++)
	  compileConstant(values[i], target);
	return;
      }
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarg = (ConditionalTarget) target;
	getCode().emitGoto(getLanguage().isTrue(value) ? ctarg.ifTrue
			   : ctarg.ifFalse);
	return;
      }
    if (target instanceof StackTarget)
      {
	Type type = ((StackTarget) target).getType();
	if (type instanceof PrimType)
	  {
	    try
	      {
		String signature = type.getSignature();
		CodeAttr code = getCode();
		char sig1 = (signature == null || signature.length() != 1) ? ' '
		  : signature.charAt(0);
		if (value instanceof Number)
		  {
		    Number num = (Number) value;
		    switch (sig1)
		      {
		      case 'I':
			code.emitPushInt(num.intValue());
			return;
		      case 'S':
			code.emitPushInt(num.shortValue());
			return;
		      case 'B':
			code.emitPushInt(num.byteValue());
			return;
		      case 'J':
			code.emitPushLong(num.longValue());
			return;
		      case 'F':
			code.emitPushFloat(num.floatValue());
			return;
		      case 'D':
			code.emitPushDouble(num.doubleValue());
			return;
		      }
		  }
		if (sig1 == 'C')
		  {
		    code.emitPushInt((int) ((PrimType) type).charValue(value));
		    return;
		  }
		if (sig1 == 'Z')
		  {
		    boolean val = PrimType.booleanValue(value);
		    code.emitPushInt(val ? 1 : 0);
		    return;
		  }
	      }
	    catch (ClassCastException ex)
	      {
		// should print an ERROR.
	      }
	  }
        try
          {
            value = type.coerceFromObject(value);
          }
        catch (Exception ex)
          {
	    StringBuffer sbuf = new StringBuffer();
	    if (value == Values.empty)
	      sbuf.append("cannot convert void to ");
	    else
	      {
		sbuf.append("cannot convert literal (of type ");
                sbuf.append(value.getClass().getName());
		sbuf.append(") to ");
	      }
	    sbuf.append(type.getName());
            error('w', sbuf.toString());
         }
      }
    compileConstant(value);
    target.compileFromStack(this,
                            value == null ? target.getType()
                            : Type.make(value.getClass()));
  }


  private void dumpInitializers (Initializer inits)
  {
    for (Initializer init = Initializer.reverse(inits);
         init != null;  init = init.next)
      init.emit(this);
  }

  /** Search this Compilation for a ClassType with a given name.
   * @param name the name of the class desired
   * @return the matching ClassType, or null if none is found */
  public ClassType findNamedClass (String name)
  {
    for (int i = 0;  i < numClasses; i++)
      {
	if (name.equals (classes[i].getName ()))
	  return classes[i];
      }
    return null;
  }

  public static String classPrefixDefault = "";
  /** If non-null: a prefix for generateClassName to prepend to names. */
  public String classPrefix = classPrefixDefault;

  /** Recusive helper function to reverse order of words in hostname. */
  private static void putURLWords(String name, StringBuffer sbuf)
  {
    int dot = name.indexOf('.');
    if (dot > 0)
      {
	putURLWords(name.substring(dot+1), sbuf);
	sbuf.append('.');
	name = name.substring(0, dot);
      }
    sbuf.append(name);
  }

  /** Map a URI to a package/class name.
   * Similar to the JAXB mangling, and that in the Java language spec.
   */
  public static String mangleURI (String name)
  {
    boolean hasSlash = name.indexOf('/') >= 0;
    int len = name.length();
    if (len > 6 && name.startsWith("class:"))
      return name.substring(6);
    // Remove "http:" or "urn:".
    if (len > 5 && name.charAt(4) == ':'
	&& name.substring(0, 4).equalsIgnoreCase("http"))
      {
	name = name.substring(5);
	len -= 5;
	hasSlash = true;
      }
    else if (len > 4 && name.charAt(3) == ':'
	     && name.substring(0, 3).equalsIgnoreCase("uri"))
      {
	name = name.substring(4);
	len -= 4;
      }
    int start = 0;
    StringBuffer sbuf = new StringBuffer();
    for (;;)
      {
	int slash = name.indexOf('/', start);
	int end = slash < 0 ? len : slash;
	boolean first = sbuf.length() == 0;
	if (first && hasSlash)
	  {
	    // Remove initial "www.".
	    String host = name.substring(start, end);
	    if (end - start > 4 && host.startsWith("www."))
	      host = host.substring(4);
	    // Reverse order of words in "host" part.
	    putURLWords(host, sbuf);
	  }
	else if (start != end)
	  {
	    if (! first)
	      sbuf.append('.');
	    if (end == len)
	      {
		int dot = name.lastIndexOf('.', len);
		if (dot > start + 1 && ! first)
		  {
		    // Remove file extension:
		    int extLen = len - dot;
		    if (extLen <= 4
			|| (extLen == 5 && name.endsWith("html")))
		      {
			len -= extLen;
			end = len;
			name = name.substring(0, len);
		      }
		  }
	      }
	    sbuf.append(name.substring(start, end));
	  }
	if (slash < 0)
	  break;
	start = slash + 1;
      }
    return sbuf.toString();
  }

  public static String mangleName (String name)
  {
    return mangleName(name, -1);
  }

  public static String mangleNameIfNeeded (String name)
  {
    if (isValidJavaName(name))
      return name;
    else
      return mangleName(name, 0);
  }

  public static boolean isValidJavaName(String name)
  {
    int len = name.length();
    if (len == 0 || ! Character.isJavaIdentifierStart(name.charAt(0)))
      return false;
    for (int i = len;  --i > 0; )
      if (! Character.isJavaIdentifierPart(name.charAt(i)))
	return false;
    return true;
  }

  public static String mangleName (String name, boolean reversible)
  {
    return mangleName(name, reversible ? 1 : -1);
  }

  /** Convert a string to a safe Java identifier.
   * @param reversible if we should use an invertible mapping.
   * @param kind -1 - non-reversible;
   *  0: reversible, except that '$' is not mapped;
   *  1: reversible
   */
  public static String mangleName (String name, int kind)
  {
    boolean reversible = kind >= 0;
    int len = name.length ();
    if (len == 6 && name.equals("*init*")) // Constructor methods.
      return "<init>";
    StringBuffer mangled = new StringBuffer (len);
    boolean upcaseNext = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt(i);
	if (upcaseNext)
	  {
	    ch = Character.toTitleCase(ch);
	    upcaseNext = false;
	  }
	if (Character.isDigit(ch))
	  {
	    if (i == 0)
	      mangled.append("$N");
	    mangled.append(ch);
	  }
	else if (Character.isLetter(ch) || ch == '_')
	  mangled.append(ch);
	else if (ch == '$')
	  mangled.append(kind > 1 ? "$$" : "$");
	else
	  {
	    switch (ch)
	      {
	      case '+':  mangled.append("$Pl");  break;
	      case '-':
		if (reversible)
		  mangled.append("$Mn");
		else
		  {
		    char next = i + 1 < len ? name.charAt(i+1) : '\0';
		    if (next == '>')
		      {
			mangled.append("$To$");
			i++;
		      }
		    else if (! Character.isLowerCase(next))
		      mangled.append("$Mn");
		  }
		break;
	      case '*':  mangled.append("$St");  break;
	      case '/':  mangled.append("$Sl");  break;
	      case '=':  mangled.append("$Eq");  break;
	      case '<':  mangled.append("$Ls");  break;
	      case '>':  mangled.append("$Gr");  break;
	      case '@':  mangled.append("$At");  break;
	      case '~':  mangled.append("$Tl");  break;
	      case '%':  mangled.append("$Pc");  break;
	      case '.':  mangled.append("$Dt");  break;
	      case ',':  mangled.append("$Cm");  break;
	      case '(':  mangled.append("$LP");  break;
	      case ')':  mangled.append("$RP");  break;
	      case '[':  mangled.append("$LB");  break;
	      case ']':  mangled.append("$RB");  break;
	      case '{':  mangled.append("$LC");  break;
	      case '}':  mangled.append("$RC");  break;
	      case '\'': mangled.append("$Sq");  break;
	      case '"':  mangled.append("$Dq");  break;
	      case '&':  mangled.append("$Am");  break;
	      case '#':  mangled.append("$Nm");  break;
	      case '?':
		char first = mangled.length() > 0 ? mangled.charAt(0) : '\0';
		if (! reversible
		    && i + 1 == len && Character.isLowerCase(first))
		  {
		    mangled.setCharAt(0, Character.toTitleCase(first));
		    mangled.insert(0, "is");
		  }
		else
		  mangled.append("$Qu");
		break;
	      case '!':  mangled.append("$Ex");  break;
	      case ':':  mangled.append("$Cl");  break;
	      case ';':  mangled.append("$SC");  break;
	      case '^':  mangled.append("$Up");  break;
	      case '|':  mangled.append("$VB");  break;
	      default:
		mangled.append('$');
		mangled.append(Character.forDigit ((ch >> 12) & 15, 16));
		mangled.append(Character.forDigit ((ch >>  8) & 15, 16));
		mangled.append(Character.forDigit ((ch >>  4) & 15, 16));
		mangled.append(Character.forDigit ((ch      ) & 15, 16));
	      }
	    if (! reversible)
	      upcaseNext = true;
	  }
      }
    String mname = mangled.toString ();
    return mname.equals(name) ? name : mname;
  }

  /** Demangle a three-character mangling starting with '$'.
   * UNFINISHED!
   */
  public static char demangle2(char char1, char char2)
  {
    switch (char1 << 16 | char2)
      {
      case 'A' << 16 | 'm':  return '&';
      case 'A' << 16 | 't':  return '@';
      case 'C' << 16 | 'l':  return ':';
      case 'C' << 16 | 'm':  return ',';
      case 'D' << 16 | 'q':  return '\"';
      case 'D' << 16 | 't':  return '.';
      case 'E' << 16 | 'q':  return '=';
      case 'E' << 16 | 'x':  return '!';
      case 'G' << 16 | 'r':  return '>';
      case 'L' << 16 | 'B':  return '[';
      case 'L' << 16 | 'C':  return '{';
      case 'L' << 16 | 'P':  return '(';
      case 'L' << 16 | 's':  return '<';
      case 'M' << 16 | 'c':  return '%';
      case 'M' << 16 | 'n':  return '-';
      case 'N' << 16 | 'm':  return '#';
      case 'P' << 16 | 'c':  return '%';
      case 'P' << 16 | 'l':  return '+';
      case 'Q' << 16 | 'u':  return '?';
      case 'R' << 16 | 'B':  return ']';
      case 'R' << 16 | 'C':  return '}';
      case 'R' << 16 | 'P':  return ')';
      case 'S' << 16 | 'C':  return ';';
      case 'S' << 16 | 'l':  return '/';
      case 'S' << 16 | 'q':  return '\\';
      case 'S' << 16 | 't':  return '*';
      case 'T' << 16 | 'l':  return '~';
      case 'U' << 16 | 'p':  return '^';
      case 'V' << 16 | 'B':  return '|';
      }
    return (char) (-1);
  }

  public static String demangleName(String name)
  {
    return demangleName(name, false);
  }

  public static String demangleName(String name, boolean reversible)
  {
    StringBuffer sbuf = new StringBuffer();
    int len = name.length();
    boolean mangled = false;
    boolean predicate = false;
    boolean downCaseNext = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt(i);
	if (downCaseNext && ! reversible)
	  {
	    ch = Character.toLowerCase(ch);
	    downCaseNext = false;
	  }
	char d;
	if (!reversible
	    && ch == 'i' && i == 0 && len > 2 && name.charAt(i+1) == 's'
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
	else if (! reversible && i > 1
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

  /** Generate an unused class name.
   * @param hint the requested name (or prefix)
   * @return a unique class name.
   */
  public String generateClassName (String hint)
  {
    hint = mangleName(hint, true);
    if (mainClass != null)
      hint = mainClass.getName() + '$' + hint;
    else if (classPrefix != null)
      hint = classPrefix + hint;
    if (findNamedClass (hint) == null)
      return hint;
    for (int i = 0;  ; i++)
      {
	String new_hint = hint + i;
	if (findNamedClass (new_hint) == null)
	  return new_hint;
      }
  }

  public Compilation (boolean immediate, SourceMessages messages)
  {
    this(messages);
    this.immediate = immediate;
  }

  public Compilation (SourceMessages messages)
  {
    this.messages = messages;
    lexical = new NameLookup(getLanguage());
  }

  public Compilation (Language language, SourceMessages messages)
  {
    this.language = language;
    this.messages = messages;
    lexical = new NameLookup(language);
  }

  public Compilation (Language language, SourceMessages messages,
		      NameLookup lexical)
  {
    this.language = language;
    this.messages = messages;
    this.lexical = lexical;
  }

  /** Shared processing for both compiling/eval. */
  public void walkModule (ModuleExp mexp)
  {
    if (debugPrintExpr)
      {
	OutPort dout = OutPort.errDefault();
	dout.println("[Module:" + mexp.getName());
	mexp.print(dout);
	dout.println(']');
	dout.flush();
      }

    InlineCalls.inlineCalls(mexp, this);
    PushApply.pushApply(mexp);
    ChainLambdas.chainLambdas(mexp, this);
    FindTailCalls.findTailCalls(mexp, this);
  }

  public void outputClass (String directory) throws IOException
  {
    char dirSep = File.separatorChar;
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	String out_name
	  = (directory + clas.getName().replace('.', dirSep)
	     + ".class");
	String parent = new File(out_name).getParent();
	if (parent != null)
	  new File(parent).mkdirs();
	clas.writeToFile(out_name);

        clas.cleanupAfterCompilation();
      }

    minfo.comp = null;
    mainLambda.body = null;
    mainLambda = null;
    litTable = null;
  }

  public void compileToArchive (ModuleExp mexp, String fname)
    throws java.io.IOException
  {
    boolean makeJar = false;
    if (fname.endsWith(".zip"))
      makeJar = false;
    else if (fname.endsWith(".jar"))
      makeJar = true;
    else
      {
	fname = fname + ".zip";
	makeJar = false;
      }

    process(COMPILED);

    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipOutputStream zout;
    /* #ifdef JAVA2 */
    if (makeJar)
      zout = new java.util.jar.JarOutputStream(new FileOutputStream(zar_file));
    else
    /* #endif */
      {
	zout = new ZipOutputStream (new FileOutputStream (zar_file));
	zout.setMethod(ZipOutputStream.STORED); // no compression
      }

    byte[][] classBytes = new byte[numClasses][];
    CRC32 zcrc = new CRC32();
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	classBytes[iClass] = clas.writeToArray ();
	ZipEntry zent = new ZipEntry(clas.getName ().replace ('.', '/')
				     + ".class");

	zent.setSize(classBytes[iClass].length);
	zcrc.reset();
	zcrc.update(classBytes[iClass], 0, classBytes[iClass].length);
	zent.setCrc(zcrc.getValue());

	zout.putNextEntry (zent);
	zout.write (classBytes[iClass]);
      }
    zout.close ();
  }

  // FIXME - make this settable, as it does make .class files bigger.
  public static boolean emitSourceDebugExtAttr = true;

  public void addClass (ClassType new_class)
  {
    if (mainLambda.filename != null)
      {
	if (emitSourceDebugExtAttr)
	  new_class.setStratum(getLanguage().getName());
	new_class.setSourceFile(mainLambda.filename);
      }
    if (classes == null)
      classes = new ClassType[20];
    else if (numClasses >= classes.length)
      {
	ClassType[] new_classes = new ClassType[2 * classes.length];
	System.arraycopy (classes, 0, new_classes, 0, numClasses);
	classes = new_classes;
      }
    new_class.access_flags |= Access.PUBLIC|Access.SUPER;
    if (new_class == mainClass && numClasses > 0)
      {
        // Ensure mainClass is written first when writing an archive.
        new_class = classes[0];
        classes[0] = mainClass;
      }
    classes[numClasses++] = new_class;
    /* #ifdef JAVA5 */
    // new_class.setClassfileVersionJava5();
    /* #endif */
  }

  public void addMainClass (ModuleExp module)
  {
    mustCompile = true;

    mainClass = module.classFor(this);

    ClassType type = mainClass;
    ClassType[] interfaces = module.getInterfaces();
    if (interfaces != null)
      type.setInterfaces(interfaces);
    ClassType sup = module.getSuperType();
    if (sup == null)
      {
        if (generateApplet)
	  sup = typeApplet;
	else if (generateServlet)
	  sup = typeServlet;
	else
	  sup = getModuleType();
      }
    if (! generateServlet && ! immediate)
      type.addInterface(typeRunnable);
    type.setSuper(sup);

    module.type = type;
    addClass(type);
    getConstructor(mainClass, module);
  }

  public final Method getConstructor (LambdaExp lexp)
  {
    return getConstructor(lexp.getHeapFrameType(), lexp);
  }

  public static final Method getConstructor (ClassType clas, LambdaExp lexp)
  {
    Method meth = clas.getDeclaredMethod("<init>", 0);
    if (meth != null)
      return meth;
    Type[] args;
    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	args = new Type[1];
	args[0] = lexp.staticLinkField.getType();
      }
    else
      args = apply0args;
    return clas.addMethod("<init>", Access.PUBLIC, args, Type.void_type);
  }

  public final void generateConstructor (LambdaExp lexp)
  {
    generateConstructor (lexp.getHeapFrameType(), lexp);
  }

  public final void generateConstructor (ClassType clas, LambdaExp lexp)
  {
    Method save_method = method;
    Variable callContextSave = callContextVar;
    callContextVar = null;
    ClassType save_class = curClass;
    curClass = clas;
    Method constructor_method = getConstructor(clas, lexp);
    clas.constructor = constructor_method;
    method = constructor_method;
    CodeAttr code = constructor_method.startCode();

    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	code.emitPushThis();
	code.emitLoad(code.getCurrentScope().getVariable(1));
	code.emitPutField(lexp.staticLinkField);
      }
    Method superConstructor
      = clas.getSuperclass().addMethod("<init>", Access.PUBLIC,
				       apply0args, Type.void_type);
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);

    if (curClass == mainClass && ! immediate)
      {
	code.emitPushThis();
	code.emitInvokeStatic(ClassType.make("gnu.expr.ModuleInfo")
                              .getDeclaredMethod("register", 1));
      }

    if (lexp.initChain != null)
      {
	// Create dummy lambda, for its closureEnv.  This may be needed
	// if init.value contains a reference that uses our heap frame.
	LambdaExp save = curLambda;
	curLambda = new LambdaExp();
	curLambda.closureEnv = code.getArg(0);
	curLambda.outer = save;
        Initializer init;
	while ((init = lexp.initChain) != null)
	  {
	    lexp.initChain = null;
	    dumpInitializers(init);
	  }
	curLambda = save;
      }

    if (lexp instanceof ClassExp)
      {
	ClassExp cexp = (ClassExp) lexp;
	callInitMethods(cexp.getCompiledClassType(this), new Vector(10));
      }

    code.emitReturn();
    method = save_method;
    curClass = save_class;
    callContextVar = callContextSave;
  }

  /** In an <init> for a generated ClassExp, emit $finit$ calls.
   * This recursively traverses superclasses, and also calls their $finit$.
   * @param clas Class to search for $finit$, and to search supertypes.
   * @param seen array of seen classes, to avoid duplicate $finit$ calls.
   */
  void callInitMethods (ClassType clas, Vector seen)
  {
    if (clas == null)
      return;

    String name = clas.getName();
    if ("java.lang.Object".equals(name))
      return;
    // Check for duplicates.
    for (int i = seen.size();  --i >= 0; )
      if (((ClassType) seen.elementAt(i)).getName() == name)
	return;
    seen.addElement(clas);

    // Recusive call to emit $finit$ of super-types.  However, don't do that
    // for clas.getSuperclass(), because our <init> will automatically call
    // the super-class's <init>, which will call its $finit$.
    ClassType[] interfaces = clas.getInterfaces();
    if (interfaces != null)
      {
	int n = interfaces.length;
	for (int i = 0;  i < n;  i++)
	  callInitMethods(interfaces[i], seen);
      }

    int clEnvArgs = 1;
    if (clas instanceof PairClassType)
      clas = ((PairClassType) clas).instanceType;
    else if (clas.isInterface())
      {
	try
	  {
	    clas = ((ClassType)
		    Type.make(Class.forName(clas.getName() + "$class")));
	  }
	catch (Throwable ex)
	  {
	    return;
	  }
      }
    else
      clEnvArgs = 0;
    Method meth = clas.getDeclaredMethod("$finit$", clEnvArgs);
    if (meth != null)
      {
	CodeAttr code = getCode();
	code.emitPushThis();
	code.emitInvoke(meth);
      }
  }

  public void generateMatchMethods(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    Method save_method = method;
    ClassType save_class = curClass;
    ClassType procType = typeModuleMethod;
    curClass = lexp.getHeapFrameType();
    if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
      curClass = moduleClass;
    CodeAttr code = null;
    for (int i = 0;  i <= 5; i++)
      {
	boolean needThisMatch = false;
	SwitchState aswitch = null;
	String mname = null;
	Type[] matchArgs = null;
	for (int j = numApplyMethods;  --j >= 0; )
	  {
	    LambdaExp source = (LambdaExp) lexp.applyMethods.elementAt(j);
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    Method[] primMethods = source.primMethods;
	    int numMethods = primMethods.length;
	    boolean varArgs = source.max_args < 0
	      || source.max_args >= source.min_args + numMethods;
	    int methodIndex;
	    if (i < 5) // Handling match0 .. match4
	      {
		methodIndex = i - source.min_args;
		if (methodIndex < 0 || methodIndex >= numMethods
		    || (methodIndex == numMethods - 1 && varArgs))
		  continue;
		numMethods = 1;
		varArgs = false;
	      }
	    else // Handling matchN
	      {
		methodIndex = 5 - source.min_args;
		if (methodIndex > 0 && numMethods <= methodIndex && ! varArgs)
		  continue;
		methodIndex = numMethods-1;
	      }
	    if (! needThisMatch)
	      {
		// First LambdaExp we seen suitable for this i.
		if (i < 5)
		  {
		    mname = "match"+i;
		    matchArgs = new Type[i + 2];
		    for (int k = i;  k >= 0;  k--)
		      matchArgs[k+1] = typeObject;
		    matchArgs[i+1] = typeCallContext;
		  }
		else
		  {
		    mname = "matchN";
		    matchArgs = new Type[3];
		    matchArgs[1] = objArrayType;
		    matchArgs[2] = typeCallContext;
		  }
		matchArgs[0] = procType;
		method = curClass.addMethod (mname, matchArgs, Type.int_type,
					     Access.PUBLIC);
		code = method.startCode();

		code.emitLoad(code.getArg(1)); // method
		code.emitGetField(procType.getField("selector"));
		aswitch = new SwitchState(code);

		needThisMatch = true;
	      }

	    aswitch.addCase(source.getSelectorValue(this), code);

	    int line = source.getLine();
	    if (line > 0)
	      code.putLineNumber(source.getFile(), line);

	    Variable ctxVar = code.getArg(i == 5 ? 3 : i+2);

	    if (i < 5)
	      {
		Declaration var = source.firstDecl();
		for (int k = 1;  k <= i;  k++)
		  {
		    code.emitLoad(ctxVar);
		    code.emitLoad(code.getArg(k+1));
		    Type ptype = var.getType();
		    if (ptype != Type.pointer_type)
		      {
			if (ptype instanceof TypeValue)
			  {
			    Label trueLabel = new Label(code),
			      falseLabel = new Label(code);
			    ConditionalTarget ctarget =
			      new ConditionalTarget(trueLabel, falseLabel,
						    getLanguage());
			    code.emitDup();
			    ((TypeValue) ptype).emitIsInstance(null, this,
							       ctarget);
			    falseLabel.define(code);
			    code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|k);
			    code.emitReturn();
			    trueLabel.define(code);
			  }
			else if (ptype instanceof ClassType
			    && ptype != Type.pointer_type
			    && ptype != Type.tostring_type)  // FIXME
			  {
			    code.emitDup();
			    ptype.emitIsInstance(code);
			    code.emitIfIntEqZero();
			    code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|k);
			    code.emitReturn();
			    code.emitFi();
			  }
		      }
		    code.emitPutField(typeCallContext.getField("value"+k));
		    var = var.nextDecl();
		  }
	      }
	    else
	      {
		// FIXME - need to check
		code.emitLoad(ctxVar);
		code.emitLoad(code.getArg(2));
		code.emitPutField(typeCallContext.getField("values"));
	      }
	    code.emitLoad(ctxVar);
	    if (defaultCallConvention < Compilation.CALL_WITH_CONSUMER)
	      code.emitLoad(code.getArg(1)); // proc
	    else
	      code.emitLoad(code.getArg(0)); // this (module)
	    code.emitPutField(procCallContextField);
	    code.emitLoad(ctxVar);
	    if (defaultCallConvention >= CALL_WITH_CONSUMER)
	      code.emitPushInt(source.getSelectorValue(this)+methodIndex);
	    else
	      code.emitPushInt(i);
	    code.emitPutField(pcCallContextField);
	    code.emitPushInt(0);
	    code.emitReturn();
          }
	if (needThisMatch)
	  {
	    aswitch.addDefault(code);
	    int nargs = i > 4 ? 2 : i + 1;
	    nargs++;
	    for (int k = 0;  k <= nargs;  k++)
	      code.emitLoad(code.getArg(k));
	    Method defMethod = (typeModuleBody
				.getDeclaredMethod(mname, matchArgs.length));
	    code.emitInvokeSpecial(defMethod);
	    code.emitReturn();
	    aswitch.finish(code);
	  }
      }
    method = save_method;
    curClass = save_class;
  }

  /** Generate ModuleBody's <tt>apply(CallContext)</tt> method
   * Use the <tt>applyMethods</tt> vector, which contains methods that
   * implement the (public, readable) methods of the current module. */
  public void generateApplyMethodsWithContext(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    ClassType save_class = curClass;
    curClass = lexp.getHeapFrameType();
    if (! (curClass.getSuperclass().isSubtype(typeModuleWithContext)))
      curClass = moduleClass;
    ClassType procType = typeModuleMethod;
    Method save_method = method;
    CodeAttr code = null;
    Type[] applyArgs = { typeCallContext };

    // First LambdaExp we seen suitable for this i.
    method = curClass.addMethod ("apply", applyArgs,
				 (Type) Type.void_type,
				 Access.PUBLIC);
    code = method.startCode();
    Variable ctxVar = code.getArg(1);

    code.emitLoad(ctxVar);
    code.emitGetField(pcCallContextField);
    SwitchState aswitch = new SwitchState(code);

    for (int j = 0;  j < numApplyMethods;  ++j)
      {
	LambdaExp source = (LambdaExp) lexp.applyMethods.elementAt(j);
	Method[] primMethods = source.primMethods;
	int numMethods = primMethods.length;

	for (int i = 0; i < numMethods; i++)
	  {
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    boolean varArgs
	      = (i == numMethods - 1
		 && (source.max_args < 0
		     || source.max_args >= source.min_args + numMethods));
	    int methodIndex = i;

	    aswitch.addCase(source.getSelectorValue(this) + i, code);

	    int line = source.getLine();
	    if (line > 0)
	      code.putLineNumber(source.getFile(), line);

	    Method primMethod = primMethods[methodIndex];
	    Type[] primArgTypes = primMethod.getParameterTypes();
	    int singleArgs = source.min_args+methodIndex;
	    Variable counter = null;
	    int pendingIfEnds = 0;

	    if (i > 4 && numMethods > 1) // FIXME
	      {
		counter = code.addLocal(Type.int_type);
		code.emitLoad(ctxVar);
		code.emitGetField(typeCallContext.getDeclaredField("count"));
		if (source.min_args != 0)
		  {
		    code.emitPushInt(source.min_args);
		    code.emitSub(Type.int_type);
		  }
		code.emitStore(counter);
	      }

	    int needsThis = primMethod.getStaticFlag() ? 0 : 1;
            int explicitFrameArg
              = singleArgs + (varArgs ? 2 : 1) < primArgTypes.length ? 1 : 0;
	    if (needsThis + explicitFrameArg > 0)
	      {
		code.emitPushThis();
		if (curClass == moduleClass && mainClass != moduleClass)
		  code.emitGetField(moduleInstanceMainField);
	      }

	    Declaration var = source.firstDecl();
	    for (int k = 0; k < singleArgs;  k++)
	      {
		if (counter != null && k >= source.min_args)
		  {
		    code.emitLoad(counter);
		    code.emitIfIntLEqZero();
		    code.emitLoad(ctxVar);
		    code.emitInvoke(primMethods[k - source.min_args]);
		    code.emitElse();
		    pendingIfEnds++;
		    code.emitInc(counter, (short) (-1));
		  }

		code.emitLoad(ctxVar);
		if (k <= 4 && ! varArgs && source.max_args <= 4)
		  code.emitGetField(typeCallContext
				    .getDeclaredField("value"+(k+1)));
		else
		  {
		    code.emitGetField(typeCallContext
				      .getDeclaredField("values"));
		    code.emitPushInt(k);
		    code.emitArrayLoad(Type.pointer_type);
		  }
		Type ptype = var.getType();
		if (ptype != Type.pointer_type)
		  CheckedTarget.emitCheckedCoerce(this, source,
						  k+1, ptype);
		var = var.nextDecl();
	      }

	    if (varArgs)
	      {
		Type lastArgType = primArgTypes[explicitFrameArg+singleArgs];
		if (lastArgType instanceof ArrayType)
		  {
		    Type elType
		      = ((ArrayType) lastArgType).getComponentType();
		    boolean mustConvert
		      = ! "java.lang.Object".equals(elType.getName());
		    if (mustConvert)
		      new Error("not implemented mustConvert restarg");
		    code.emitLoad(ctxVar);
		    code.emitPushInt(singleArgs);
		    code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsArray", 1));
		  }
		else if ("gnu.lists.LList".equals
			 (lastArgType.getName()))
		  {	
		    code.emitLoad(ctxVar);
		    code.emitPushInt(singleArgs);
		    code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsList", 1));
		  }
		else if (lastArgType == typeCallContext)
		  code.emitLoad(ctxVar);
		else
		  throw new RuntimeException("unsupported #!rest type:"+lastArgType);
              }
	    code.emitLoad(ctxVar); // get $ctx
	    code.emitInvoke(primMethod);
	    while (--pendingIfEnds >= 0)
	      code.emitFi();
	    if (defaultCallConvention < Compilation.CALL_WITH_CONSUMER)
	      Target.pushObject.compileFromStack(this,
						 source.getReturnType());
	    code.emitReturn();
          }
      }
    aswitch.addDefault(code);
    Method errMethod = typeModuleMethod.getDeclaredMethod("applyError", 0);
    code.emitInvokeStatic(errMethod);
    code.emitReturn();
    aswitch.finish(code);
    method = save_method;
    curClass = save_class;
  }

  /** Generate ModuleBody's <tt>apply0</tt>...<tt>applyN</tt> methods.
   * Use the <tt>applyMethods</tt> vector, which contains methods that
   * implement the (public, readable) methods of the current module.
   */
  public void generateApplyMethodsWithoutContext(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    ClassType save_class = curClass;
    curClass = lexp.getHeapFrameType();
    ClassType procType = typeModuleMethod;
    if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
      curClass = moduleClass;
    Method save_method = method;
    CodeAttr code = null;
    for (int i = defaultCallConvention >= Compilation.CALL_WITH_CONSUMER
	   ? 5 : 0;
	 i < 6; i++)
      {
	// If i < 5, generate the method named ("apply"+i);
	// else generate "applyN".
	boolean needThisApply = false;
	SwitchState aswitch = null;
	String mname = null;
	Type[] applyArgs = null;

	for (int j = numApplyMethods;  --j >= 0; )
	  {
	    LambdaExp source = (LambdaExp) lexp.applyMethods.elementAt(j);
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    Method[] primMethods = source.primMethods;
	    int numMethods = primMethods.length;
	    boolean varArgs = source.max_args < 0
	      || source.max_args >= source.min_args + numMethods;
	    int methodIndex;
	    boolean skipThisProc = false;
	    if (i < 5) // Handling apply0 .. apply4
	      {
		methodIndex = i - source.min_args;
		if (methodIndex < 0 || methodIndex >= numMethods
		    || (methodIndex == numMethods - 1 && varArgs))
		  skipThisProc = true;
		numMethods = 1;
		varArgs = false;
	      }
	    else // Handling applyN
	      {
		methodIndex = 5 - source.min_args;
		if (methodIndex > 0 && numMethods <= methodIndex && ! varArgs)
		  skipThisProc = true;
		methodIndex = numMethods-1;
	      }
	    if (skipThisProc)
	      continue;
	    if (! needThisApply)
	      {
		// First LambdaExp we seen suitable for this i.
		if (i < 5)
		  {
		    mname =  "apply"+i;
		    applyArgs = new Type[i + 1];
		    for (int k = i;  k > 0;  k--)
		      applyArgs[k] = typeObject;
		  }
		else
		  {
		    mname = "applyN";
		    applyArgs = new Type[2];
		    applyArgs[1] = objArrayType;
		  }
		applyArgs[0] = procType;
		method = curClass.addMethod (mname, applyArgs,
					     defaultCallConvention >= Compilation.CALL_WITH_CONSUMER ? (Type) Type.void_type : (Type) Type.pointer_type,
					     Access.PUBLIC);
		code = method.startCode();

		code.emitLoad(code.getArg(1)); // method
		code.emitGetField(procType.getField("selector"));
		aswitch = new SwitchState(code);

		needThisApply = true;
	      }

	    aswitch.addCase(source.getSelectorValue(this), code);

	    int line = source.getLine();
	    if (line > 0)
	      code.putLineNumber(source.getFile(), line);

	    Method primMethod = primMethods[methodIndex];
	    Type[] primArgTypes = primMethod.getParameterTypes();
	    int singleArgs = source.min_args+methodIndex;
	    Variable counter = null;
	    int pendingIfEnds = 0;

	    if (i > 4 && numMethods > 1)
	      {
		counter = code.addLocal(Type.int_type);
		code.emitLoad(code.getArg(2));
		code.emitArrayLength();
		if (source.min_args != 0)
		  {
		    code.emitPushInt(source.min_args);
		    code.emitSub(Type.int_type);
		  }
		code.emitStore(counter);
	      }

	    int needsThis = primMethod.getStaticFlag() ? 0 : 1;
            int explicitFrameArg
              = singleArgs + (varArgs ? 1 : 0) < primArgTypes.length ? 1 : 0;
	    if (needsThis + explicitFrameArg > 0)
	      {
		code.emitPushThis();
		if (curClass == moduleClass && mainClass != moduleClass)
		  code.emitGetField(moduleInstanceMainField);
	      }

	    Declaration var = source.firstDecl();
	    for (int k = 0; k < singleArgs;  k++)
	      {
		if (counter != null && k >= source.min_args)
		  {
		    code.emitLoad(counter);
		    code.emitIfIntLEqZero();
		    code.emitInvoke(primMethods[k - source.min_args]);
		    code.emitElse();
		    pendingIfEnds++;
		    code.emitInc(counter, (short) (-1));
		  }

		Variable pvar = null;
		if (i <= 4) // apply'i method
		  {
		    pvar = code.getArg(k + 2);
		    code.emitLoad(pvar);
		  }
		else // applyN method
		  {
		    // Load Object[]args value:
		    code.emitLoad(code.getArg(2));
		    code.emitPushInt(k);
		    code.emitArrayLoad(Type.pointer_type);
		  }
		Type ptype = var.getType();
		if (ptype != Type.pointer_type)
		  CheckedTarget.emitCheckedCoerce(this, source,
						  k+1, ptype, pvar);
		var = var.nextDecl();
	      }

	    if (varArgs)
	      {
		Type lastArgType = primArgTypes[explicitFrameArg+singleArgs];
		if (lastArgType instanceof ArrayType)
		  {
		    Type elType
		      = ((ArrayType) lastArgType).getComponentType();
		    boolean mustConvert
		      = ! "java.lang.Object".equals(elType.getName());
		    if (singleArgs == 0 && ! mustConvert)
		      code.emitLoad(code.getArg(2)); // load args array.
		    else
		      {
			code.pushScope();
			if (counter == null)
			  {
			    counter = code.addLocal(Type.int_type);
			    code.emitLoad(code.getArg(2));
			    code.emitArrayLength();
			    if (singleArgs != 0)
			      {
				code.emitPushInt(singleArgs);
				code.emitSub(Type.int_type);
			      }
			    code.emitStore(counter);
			  }
			code.emitLoad(counter);
			code.emitNewArray(elType);
			Label testLabel = new Label(code);
			code.emitGoto(testLabel);
			Label loopTopLabel = new Label(code);
			loopTopLabel.define(code);

			code.emitDup(1); // new array
			code.emitLoad(counter);
			code.emitLoad(code.getArg(2));
			code.emitLoad(counter);
			if (singleArgs != 0)
			  {
			    code.emitPushInt(singleArgs);
			    code.emitAdd(Type.int_type);
			  }
			code.emitArrayLoad(Type.pointer_type);
			if (mustConvert)
			  {
			    CheckedTarget.emitCheckedCoerce
			      (this, source, source.getName(),
			       0, elType, null);
			  }
			code.emitArrayStore(elType);
			testLabel.define(code);
			code.emitInc(counter, (short) (-1));
			code.emitLoad(counter);
			code.emitGotoIfIntGeZero(loopTopLabel);
			code.popScope();	
		      }
		  }
		else if ("gnu.lists.LList".equals
			 (lastArgType.getName()))
		  {	
		    code.emitLoad(code.getArg(2)); // load args array.
		    code.emitPushInt(singleArgs);
		    code.emitInvokeStatic(Compilation.makeListMethod);
		  }
		else if (lastArgType == typeCallContext)
		  code.emitLoad(code.getArg(2));
		else
		  throw new RuntimeException("unsupported #!rest type:"+lastArgType);
              }
	    code.emitInvoke(primMethod);
	    while (--pendingIfEnds >= 0)
	      code.emitFi();
	    if (defaultCallConvention < Compilation.CALL_WITH_CONSUMER)
	      Target.pushObject.compileFromStack(this,
						 source.getReturnType());
	    code.emitReturn();
          }
	if (needThisApply)
	  {
	    aswitch.addDefault(code);
	    if (defaultCallConvention >= Compilation.CALL_WITH_CONSUMER)
	      {
		Method errMethod
		  = typeModuleMethod.getDeclaredMethod("applyError", 0);
		code.emitInvokeStatic(errMethod);
	      }
	    else
	      {
		int nargs = i > 4 ? 2 : i + 1;
		nargs++;
		for (int k = 0; k < nargs;  k++)
		  code.emitLoad(code.getArg(k));
		code.emitInvokeSpecial(typeModuleBody.getDeclaredMethod(mname, applyArgs));
	      }
	    code.emitReturn();
	    aswitch.finish(code);
	  }
      }
    method = save_method;
    curClass = save_class;
  }

  private Method startClassInit ()
  {
    method = curClass.addMethod ("<clinit>", apply0args, Type.void_type,
				 Access.PUBLIC|Access.STATIC);

    CodeAttr code = method.startCode();

    if (generateMain || generateApplet || generateServlet)
      {
	ClassType languageType
	  = (ClassType) Type.make(getLanguage().getClass());
	Method registerMethod
	  = languageType.getDeclaredMethod("registerEnvironment", 0);
	if (registerMethod != null)
	  code.emitInvokeStatic(registerMethod);
      }
    return method;
  }

  /** Parse/walk/compile this module as needed and requested.
   * This method does not process any dependent modules (expect indirectly,
   * such as may be done by a require form).
   * @param wantedState the desired value of getState().
   */
  public void process (int wantedState)
  {
    Compilation saveCompilation = Compilation.getCurrent();
    try
      {
        Compilation.setCurrent(this);
        ModuleExp mexp = getModule();
        if (wantedState >= BODY_PARSED && getState() < BODY_PARSED-1)
          {
            setState(BODY_PARSED-1);
            language.parse(this, 0);
            lexer.close();
            lexer = null;
            if (pendingImports != null)
              return;
          }
        if (wantedState >= RESOLVED && getState() < RESOLVED)
          {
            addMainClass(mexp);
            language.resolve(this);
            setState(messages.seenErrors() ? ERROR_SEEN : RESOLVED);
          }
        if (wantedState >= WALKED && getState() < WALKED)
          {
            walkModule(mexp);
            setState(messages.seenErrors() ? ERROR_SEEN : WALKED);
          }
        if (wantedState >= COMPILE_SETUP && getState() < COMPILE_SETUP)
          {
            litTable = new LitTable(this);
            mexp.setCanRead(true);
            FindCapturedVars.findCapturedVars(mexp, this);
            mexp.allocFields(this);
            mexp.allocChildMethods(this);
            setState(messages.seenErrors() ? ERROR_SEEN : COMPILE_SETUP);
          }
        if (wantedState >= COMPILED && getState() < COMPILED)
          {
            generateBytecode();
            setState(messages.seenErrors() ? ERROR_SEEN : COMPILED);
          }
        if (wantedState >= CLASS_WRITTEN && getState() < CLASS_WRITTEN)
          { 
            // FIXME should avoid refereence to kawa.repl
            String directory = kawa.repl.compilationDirectory;
            if (directory == null || directory.length() == 0)
              directory = "";
            else if (directory.charAt(directory.length() - 1)
                     != java.io.File.separatorChar)
              directory = directory + java.io.File.separatorChar;
            outputClass(directory);
            setState(CLASS_WRITTEN);
          }
      }
    catch (gnu.text.SyntaxException ex)
      {
        setState(ERROR_SEEN);
        if (ex.getMessages() != getMessages())
          throw new RuntimeException ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (java.io.IOException ex)
      {
        ex.printStackTrace();
        error('f', "caught "+ex);
        setState(ERROR_SEEN);
      }
    finally
      {
        Compilation.setCurrent(saveCompilation);
      }
  }

  /** The guts of compiling a module to one or more classes.
   * Assumes walkModule has been done.
   */
  void generateBytecode ()
  {
    ModuleExp module = getModule();
    if (debugPrintFinalExpr)
      {
	OutPort dout = OutPort.errDefault();
	dout.println ("[Compiling final "+module.getName()
                     + " to " + mainClass.getName() + ":");
	module.print(dout);
	dout.println(']');
	dout.flush();
      }

    ClassType neededSuper = getModuleType();
    if (mainClass.getSuperclass().isSubtype(neededSuper))
      moduleClass = mainClass;
    else
      {
	moduleClass = new ClassType(generateClassName("frame"));
	moduleClass.setSuper(neededSuper);
	addClass(moduleClass);
	generateConstructor(moduleClass, module);
      }

    curClass = module.type;
    int arg_count;
    LambdaExp saveLambda = curLambda;
    curLambda = module;
    Type[] arg_types;
    if (module.isHandlingTailCalls())
      {
	arg_count = 1;
	arg_types = new Type[1];
	arg_types[0] = typeCallContext;
      }
    else if (module.min_args != module.max_args || module.min_args > 4
	|| (fewerClasses && curClass == mainClass))
      {
	arg_count = 1;
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (typeObject);
      }
    else
      {
	arg_count = module.min_args;
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = typeObject;
      }

    CodeAttr code;
    Variable heapFrame = module.heapFrame;
    boolean staticModule = module.isStatic();
    Method apply_method;
    
    apply_method = curClass.addMethod ("run", arg_types, Type.void_type,
				       Access.PUBLIC+Access.FINAL);
    method = apply_method;
    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();
    // if (usingCPStyle())   code.addParamLocals();

    thisDecl = method.getStaticFlag() ? null : module.declareThis(module.type);
    module.closureEnv = module.thisVariable;
    module.heapFrame = module.isStatic() ? null : module.thisVariable;
    module.allocChildClasses(this);

    if (module.isHandlingTailCalls() || usingCPStyle())
      {
	Variable callStackContext = new Variable ("$ctx", typeCallContext);
	module.getVarScope().addVariableAfter(thisDecl, callStackContext);
	callStackContext.setParameter(true);
      }

    int line = module.getLine();
    if (line > 0)
      code.putLineNumber(module.getFile(), line);

    module.allocParameters(this);
    module.enterFunction(this);
    if (usingCPStyle())
      {
	loadCallContext();
        code.emitGetField(pcCallContextField);
        fswitch = new SwitchState(code);
	Label l = new Label(code);
	l.define(code);
	fswitch.addCase(0, l, code);
      }

    module.compileBody(this);
    module.compileChildMethods(this);

    Label startLiterals = null;
    Label afterLiterals = null;
    Method initMethod = null;

    if (curClass == mainClass)
      {
	Method save_method = method;

	initMethod = startClassInit();
	code = getCode();

        startLiterals = new Label(code);
        afterLiterals = new Label(code);
        code.fixupChain(afterLiterals, startLiterals);
	  
	if (staticModule)
	  {
	    generateConstructor (module);

	    code.emitNew(moduleClass);
	    code.emitDup(moduleClass);
	    code.emitInvokeSpecial(moduleClass.constructor);
	    moduleInstanceMainField
	      = moduleClass.addField("$instance", mainClass,
				     Access.STATIC|Access.PUBLIC|Access.FINAL);
	    code.emitPutStatic(moduleInstanceMainField);
	  }
        Initializer init;
        while ((init = clinitChain) != null)
          {
            clinitChain = null;
            dumpInitializers(init);
          }

	if (! immediate && module.staticInitRun())
	  {
	    code.emitGetStatic(moduleInstanceMainField);
	    code.emitInvokeInterface(typeRunnable.getDeclaredMethod("run", 0));
	  }
	code.emitReturn();

	if (moduleClass != mainClass
	    && ! staticModule && ! generateMain && ! immediate)
	  {
	    // Compare the run methods in ModuleBody.
	    method = curClass.addMethod("run", Access.PUBLIC,
					Type.typeArray0, Type.void_type);
	    code = method.startCode();
	    Variable ctxVar = code.addLocal(typeCallContext);
	    Variable saveVar = code.addLocal(typeConsumer);
	    Variable exceptionVar = code.addLocal(Type.throwable_type);
	    // ctx = CallContext.getInstance();
	    code.emitInvokeStatic(getCallContextInstanceMethod);
	    code.emitStore(ctxVar);
	    Field consumerFld = typeCallContext.getDeclaredField("consumer");
	    // save = ctx.consumer;
	    code.emitLoad(ctxVar);
	    code.emitGetField(consumerFld);
	    code.emitStore(saveVar);
	    // ctx.consumer = VoidConsumer.instance:
	    code.emitLoad(ctxVar);
	    code.emitGetStatic(ClassType.make("gnu.lists.VoidConsumer")
			       .getDeclaredField("instance"));
	    code.emitPutField(consumerFld);
	    // try {
	    code.emitTryStart(false, Type.void_type);
	    // this.apply(ctx):
	    code.emitPushThis();
	    code.emitLoad(ctxVar);
	    code.emitInvokeVirtual(save_method);
	    // exception = null
	    code.emitPushNull();
	    code.emitStore(exceptionVar);
	    // } catch (Throwable th) { exception = th; }
	    code.emitTryEnd();
	    code.emitCatchStart(exceptionVar);
	    code.emitCatchEnd();
	    code.emitTryCatchEnd();
	    // MooduleBody.runCleanup(ctx, ex, save);
	    code.emitLoad(ctxVar);
	    code.emitLoad(exceptionVar);
	    code.emitLoad(saveVar);
	    code.emitInvokeStatic(typeModuleBody
				  .getDeclaredMethod("runCleanup", 3));
	    code.emitReturn();
	  }

	method = save_method;
      }

    module.compileEnd(this);

    curLambda = saveLambda;

    if (Compilation.fewerClasses) // FIXME
      method.popScope(); // Undoes pushScope in method.initCode.

    module.heapFrame = heapFrame;  // Restore heapFrame.
    if (usingCPStyle() || (fewerClasses && curClass == mainClass))
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (startLiterals != null)
      {
	method = initMethod;
	code = getCode();

	Label endLiterals = new Label(code);
	code.fixupChain(startLiterals, endLiterals);

	try
	  {
            if (immediate)
              code.emitInvokeStatic(ClassType.make("gnu.expr.ModuleExp")
                                    .getDeclaredMethod("setupLiterals", 0));
            else
              litTable.emit();
	  }
	catch (Throwable ex)
	  {
	    error('e', "Literals: Internal error:" + ex);
	  }
	code.fixupChain(endLiterals, afterLiterals);
      }

    if (generateMain && curClass == mainClass)
      {
	Type[] args = { new ArrayType(javaStringType) };
	method = curClass.addMethod("main", Access.PUBLIC|Access.STATIC,
				    args, Type.void_type);
				    
	code = method.startCode();

	if (Shell.defaultFormatName != null)
	  {
	    code.emitPushString(Shell.defaultFormatName);
	    code.emitInvokeStatic(ClassType.make("kawa.Shell")
				  .getDeclaredMethod("setDefaultFormat", 1));
	  }
	code.emitLoad(code.getArg(0));
	code.emitInvokeStatic(typeModuleBody.getDeclaredMethod("processArgs", 1));
	if (moduleInstanceMainField != null)
	  code.emitGetStatic(moduleInstanceMainField);
	else
	  {
	    code.emitNew(curClass);
	    code.emitDup(curClass);
	    code.emitInvokeSpecial(curClass.constructor);
	  }
	code.emitInvokeVirtual(typeModuleBody.getDeclaredMethod("runAsMain", 0));
	code.emitReturn();
      }
  }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  /** If non-null, contains the value of the current CallContext. */
  Variable callContextVar;

  /** Generate code to push the current CallContext on the JVM stack. */
  public final void loadCallContext()
  {
    CodeAttr code = getCode();
    if (callContextVar == null)
      {
        code.emitInvokeStatic(getCallContextInstanceMethod);
        code.emitDup();
        callContextVar = new Variable("$ctx", typeCallContext);
        curLambda.getVarScope().addVariable(code, callContextVar);
        code.emitStore(callContextVar);
      }
    else
      code.emitLoad(callContextVar);
  }

  public void freeLocalField (Field field)
  {
    // FIXME
  }

  /** This may not make sense, except for Lisp-like languages.
   * For those, 'input' an s-expression  from the reader. */
  public Expression parse (Object input)
  {
    throw new Error("unimeplemented parse");
  }

  protected Language language;
  public Language getLanguage() { return language; }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public final ModuleExp getModule() { return mainLambda; }
  public void setModule(ModuleExp mexp) { mainLambda = mexp; }

  public boolean isStatic() { return mainLambda.isStatic(); }

  /** The same as getModule, until we allow nested modules. */
  public ModuleExp currentModule() { return current_scope.currentModule(); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled.
   */
  public void mustCompileHere ()
  {
    mustCompile = true;
  }

  public ScopeExp currentScope() { return current_scope; }

  /** Set <code>currentScope()</code>.
   * Also update the <code>nesting</code> object.
   */
  public void setCurrentScope (ScopeExp scope)
  {
    int scope_nesting = ScopeExp.nesting(scope);
    int current_nesting = ScopeExp.nesting(current_scope);
    while (current_nesting > scope_nesting)
      {
	pop(current_scope);
	current_nesting--;
      }
    ScopeExp sc = scope;
    while (scope_nesting > current_nesting)
      {
	sc = sc.outer;
	scope_nesting--;
      }
    while (sc != current_scope)
      {
	pop(current_scope);
	sc = sc.outer;
      }
    pushChain(scope, sc);
  }

  void pushChain (ScopeExp scope, ScopeExp limit)
  {
    if (scope != limit)
      {
	pushChain(scope.outer, limit);
        pushScope(scope);
        lexical.push(scope);
      }
  }

  public ModuleExp pushNewModule (Lexer lexer)
  {
    this.lexer = lexer;
    return pushNewModule(lexer.getName());
  }

  public ModuleExp pushNewModule (String filename)
  {
    ModuleExp module = new ModuleExp();
    if (filename != null)
      module.setFile(filename);
    if (Compilation.generateAppletDefault)
      module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    if (immediate)
      module.setFlag(ModuleExp.IMMEDIATE);
    mainLambda = module;
    push(module);
    return module;
  }

  public void push (ScopeExp scope)
  {
    pushScope(scope);
    lexical.push(scope);
  }

  public final void pushScope (ScopeExp scope)
  {
    if (! mustCompile
        // We set mustCompile if we see a LambdaExp - not because we must
        // but because it is usually desirable.
        && (scope.mustCompile()
            || (scope instanceof LambdaExp && ! (scope instanceof ModuleExp))))
      mustCompileHere();
    scope.outer = current_scope;
    current_scope = scope;
  }

  public void pop (ScopeExp scope)
  {
    lexical.pop(scope);
    current_scope = scope.outer;
  }

  public final void pop ()
  {
    pop(current_scope);
  }

  public void push (Declaration decl)
  {
    lexical.push(decl);
  }

  public Declaration lookup(Object name, int namespace)
  {
    return lexical.lookup(name, namespace);
  }

  /** Called for classes referenced in bytecode.
   * Since this only does something when immediate, we only care about
   * classes referenced in the bytecode when immediate.
   * It is used to ensure that we can inherit from classes defines when in
   * immediate mode (in Scheme using define-class or similar).
   */
  public void usedClass (Type type)
  {
    while (type instanceof ArrayType)
      type = ((ArrayType) type).getComponentType();
    if (! immediate || ! (type instanceof ClassType))
      return;
    ClassType clas = (ClassType) type;
    if (loader != null && clas.isExisting())
      {
	loader.addClass(clas.getReflectClass());
      }
  }

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message, Expression location)
  {
    String file = location.getFile();
    int line = location.getLine();
    int column = location.getColumn();
    if (file == null || line <= 0)
      {
        file = getFile();
        line = getLine();
        column = getColumn();
      }

    if (severity == 'w' && getBooleanOption("warn-as-error", false))
      severity = 'e';
    messages.error(severity, file, line, column, message);
  }

  public void error(char severity, String message)
  {
    if (severity == 'w' && getBooleanOption("warn-as-error", false))
      severity = 'e';
    
    messages.error(severity, getFile(), getLine(), getColumn(),
		   message);
  }

  public void error(char severity, Declaration decl, String msg1, String msg2)
  {
    if (severity == 'w' && getBooleanOption("warn-as-error", false))
      severity = 'e';
    
    String filename = getFile();
    int line = getLine();
    int column = getColumn();
    int decl_line = decl.getLine();
    if (decl_line > 0)
      {
	filename = decl.getFile();
	line = decl_line;
	column = decl.getColumn();
      }
    messages.error(severity, filename, line, column,
		   msg1 + decl.getName() + msg2);
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
  {
    error('e', message);
    return new ErrorExp (message);
  }

  public final String getFile() { return messages.getFile(); }
  public final int getLine() { return messages.getLine(); }
  public final int getColumn() { return messages.getColumn(); }

  public void setFile(String filename) { messages.setFile(filename); }
  public void setLine(int line) { messages.setLine(line); }
  public void setColumn(int column) { messages.setColumn(column); }
  public final void setLine(Expression position)
  { setLine(position.filename, position.getLine(), position.getColumn()); }

  public void setLine(String filename, int line, int column)
  {
    messages.setLine(filename, line, column);
  }

  /** A help vector for building expressions. */
  public Stack exprStack;

  public void letStart ()
  {
    pushScope(new LetExp(null));
  }

  public Declaration letVariable (Object name, Type type, Expression init)
  {
    LetExp let = (LetExp) current_scope;
    Declaration decl = let.addDeclaration(name, type);
    decl.noteValue(init);
    return decl;
  }

  public void letEnter ()
  {
    LetExp let = (LetExp) current_scope;
    int ndecls = let.countDecls();
    Expression[] inits = new Expression[ndecls];
    int i = 0;
    for (Declaration decl = let.firstDecl();
	 decl != null;
	 decl = decl.nextDecl())
      inits[i++] = decl.getValue();
    let.inits = inits;
    lexical.push(let);
  }

  public LetExp letDone (Expression body)
  {
    LetExp let = (LetExp) current_scope;
    let.body = body;
    pop(let);
    return let;
  }

  private void checkLoop()
  {
    if (((LambdaExp) current_scope).getName() != "%do%loop")
      throw new Error("internal error - bad loop state");
  }

  /** Start a new loop.
   * (We could make this implied by the first loopVaribale call ???) */
  public void loopStart()
  {
    LambdaExp loopLambda = new LambdaExp();
    Expression[] inits = { loopLambda };
    LetExp let = new LetExp(inits);
    String fname = "%do%loop";
    Declaration fdecl = let.addDeclaration(fname);
    fdecl.noteValue(loopLambda);
    loopLambda.setName(fname);
    let.outer = current_scope;
    loopLambda.outer = let;
    current_scope = loopLambda;
  }

  public Declaration loopVariable(Object name, Type type, Expression init)
  {
    checkLoop();
    LambdaExp loopLambda = (LambdaExp) current_scope;
    Declaration decl = loopLambda.addDeclaration(name, type);
    if (exprStack == null)
      exprStack = new Stack();
    exprStack.push(init);
    loopLambda.min_args++;
    return decl;
  }

  /** Done handling loop variables, and pushes them into the lexical scope.
   * Ready to parse the loop condition. */ 
  public void loopEnter ()
  {
    checkLoop();
    LambdaExp loopLambda = (LambdaExp) current_scope;
    int ninits = loopLambda.min_args;
    loopLambda.max_args = ninits;
    Expression[] inits = new Expression[ninits];
    for (int i = ninits;  --i >= 0; )
      inits[i] = (Expression) exprStack.pop();
    LetExp let = (LetExp) loopLambda.outer;
    Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
    let.setBody(new ApplyExp(new ReferenceExp(fdecl), inits));
    lexical.push(loopLambda);
  }
  public void loopCond(Expression cond)
  {
    checkLoop();
    exprStack.push(cond);
  }
  public void loopBody(Expression body)
  {
    LambdaExp loopLambda = (LambdaExp) current_scope;
    loopLambda.body = body;
  }
  public Expression loopRepeat(Expression[] exps)
  {
    LambdaExp loopLambda = (LambdaExp) current_scope;
    ScopeExp let = loopLambda.outer;
    Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
    Expression cond = (Expression) exprStack.pop();
    Expression recurse = new ApplyExp(new ReferenceExp(fdecl), exps);
    loopLambda.body = new IfExp(cond,
				new BeginExp(loopLambda.body, recurse),
				QuoteExp.voidExp);
    lexical.pop(loopLambda);
    current_scope = let.outer;
    return let;
  }

  public Expression loopRepeat ()
  {
    return loopRepeat(Expression.noExpressions);
  }

  public Expression loopRepeat (Expression exp)
  {
    Expression[] args = { exp };
    return loopRepeat(args);
  }

  /** If non-null, a helper method generated by getForNameHelper. */
  Method forNameHelper;

  public void loadClassRef (ClassType clas)
  {
    // Try an optimization
    if (clas == mainClass && mainLambda.isStatic()
        // moduleInstanceMainField may not have been set yet.
        && moduleInstanceMainField != null)
      {
        CodeAttr code = getCode();
        code.emitGetStatic(moduleInstanceMainField);
        code.emitInvokeVirtual(Type.pointer_type.getDeclaredMethod("getClass", 0));
      }
    else
      loadClassRef(clas.getName());
  }

  /** Generate code to load a named Class without initializing it.
   */
  public void loadClassRef (String className)
  {
    CodeAttr code = getCode();
    if (curClass.getClassfileMajorVersion() >= 49) // Java5 feature
      code.emitPushClass(className);
    else
      {
        code.emitPushString(className);
        code.emitInvokeStatic(getForNameHelper());
      }
  }

  /** Generate a method to find a named Class without initializing it.
   * Generate a static helper method "class$" like javac generates for
   * 'CLASS.class', but does not initialize CLASS.  Also, we don't bother
   * catching exceptions, since the JVM doesn't require us to.  I.e. generates:
   * public static class $(String name)
   * { return Class.forName(name, false,
   *                        Class.forName(THISCLASSNAME).getClassLoader()); }
   * Note that we want the result to use the same ClassLoader as the caller,
   * which is why we generate a static helper method.
   */
  public Method getForNameHelper ()
  {
    if (forNameHelper == null)
      {
	/* #ifdef JAVA2 */
	Method save_method = method;
	method = curClass.addMethod("class$", Access.PUBLIC|Access.STATIC,
				    string1Arg, typeClass);
	forNameHelper = method;
	CodeAttr code = method.startCode();
	code.emitLoad(code.getArg(0));
	code.emitPushInt(0);
	code.emitPushString(mainClass.getName());
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 1));
	code.emitInvokeVirtual(typeClass.getDeclaredMethod("getClassLoader", 0));
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 3));
	code.emitReturn();
	method = save_method;
	/* #else */
	// forNameHelper = typeClass.getDeclaredMethod("forName", 1);
	/* #endif */
      }
    return forNameHelper;
  }

  public Object resolve(Object name, boolean function)
  {
    Environment env = Environment.getCurrent();
    Symbol symbol;
    if (name instanceof String)
      symbol = env.defaultNamespace().lookup((String) name);
    else
      symbol = (Symbol) name;
    if (symbol == null)
      return null;
    if (function && getLanguage().hasSeparateFunctionNamespace())
      return env.getFunction(symbol, null);
    return env.get(symbol, null);
  }

  /** Current lexical scope - map name to Declaration. */
  public NameLookup lexical;

  protected ScopeExp current_scope;

  protected SourceMessages messages;

  private static final ThreadLocation current =
    new ThreadLocation("current-compilation");

  public static Compilation getCurrent ()
  {
    return (Compilation) current.get();
  }

  public static void setCurrent (Compilation comp)
  {
    current.set(comp);
  }

  public String toString ()
  {
    return "<compilation "+mainLambda+">";
  }
}
