// Copyright (c) 1999, 2000, 2001, 2002, 2003  Per M.A. Bothner.
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
// import java.util.jar.*; // Java2

public class Compilation
{
  /** Used by LambdaExp.getSelectorValue if need to allocate new selector. */
  int maxSelectorValue;

  public ClassType curClass;
  public ClassType mainClass;

  public LambdaExp curLambda;
  public ModuleExp mainLambda;
  public Variable thisDecl;

  /** Contains "$instance" if the module is static; otherwise null. */
  Field instanceField;

  /** If true, minimize the number of classes generated.
   * Do this even if it makes things a little slower. */
  public static boolean fewerClasses;

  /** If true, print out final expressions after optimizations etc. */
  public static boolean debugPrintFinalExpr;

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

  public static boolean usingCPStyle()
  { return defaultCallConvention == CALL_WITH_CONTINUATIONS; }
  public boolean usingTailCalls()
  { return defaultCallConvention >= CALL_WITH_TAILCALLS; }

  /** If moduleStatic > 0, (module-static #t) is implied by default.
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
  static public ClassType scmSymbolType = typeString;
  static public ClassType scmKeywordType = ClassType.make("gnu.expr.Keyword");
  static public ClassType scmSequenceType = ClassType.make("gnu.lists.Sequence");
  static public ClassType javaIntegerType = ClassType.make("java.lang.Integer");
  static public ClassType scmListType = ClassType.make("gnu.lists.LList");
  static public ClassType typePair = ClassType.make("gnu.lists.Pair");
  static public ClassType scmPairType = typePair;
  static public ClassType scmUndefinedType = ClassType.make("gnu.expr.Undefined");
  public static final ArrayType objArrayType = ArrayType.make(typeObject);
  public static final ArrayType symbolArrayType= ArrayType.make(scmSymbolType);
  static public ClassType scmNamedType = ClassType.make("gnu.mapping.Named");
  static public ClassType typeRunnable = ClassType.make("java.lang.Runnable");
  static public ClassType typeObjectType = ClassType.make("gnu.bytecode.ObjectType");
  static public ClassType typeClassType = ClassType.make("gnu.bytecode.ClassType", typeObjectType);
  static public ClassType typeProcedure
    = ClassType.make("gnu.mapping.Procedure");
  static public ClassType typeInterpreter
    = ClassType.make("gnu.expr.Interpreter");
  static public ClassType typeMacro
    = ClassType.make("kawa.lang.Macro");
  static public ClassType typeEnvironment
    = ClassType.make("gnu.mapping.Environment");
  static public ClassType typeLocation
    = ClassType.make("gnu.mapping.Location");
  static public ClassType typeSymbol
    = ClassType.make("gnu.mapping.Symbol", typeLocation);
  static public final Method getSymbolValueMethod
    = typeInterpreter.getDeclaredMethod("getSymbolValue", 1);
  static public final Method getSymbolProcedureMethod
    = typeInterpreter.getDeclaredMethod("getSymbolProcedure", 1);
  static public final Method getLocationMethod
    = typeLocation.addMethod("get", Type.typeArray0,
			    Type.pointer_type, Access.PUBLIC);
  static public final Method getProcedureBindingMethod
    = typeSymbol.addMethod("getProcedure", Type.typeArray0,
			    typeProcedure, Access.PUBLIC);
  static public final Field trueConstant
    = scmBooleanType.addField ("TRUE", scmBooleanType,
			       Access.PUBLIC|Access.STATIC); 
  static public final Field falseConstant
    = scmBooleanType.addField ("FALSE", scmBooleanType,
			       Access.PUBLIC|Access.STATIC);
  static final Field voidConstant
    = typeInterpreter.addField ("voidObject", typeObject,
				Access.PUBLIC|Access.STATIC);
  static final Field undefinedConstant
    = typeInterpreter.addField ("undefinedObject", scmUndefinedType,
				Access.PUBLIC|Access.STATIC);
  static final Field emptyConstant
    = scmListType.addField ("Empty", scmListType,
			    Access.PUBLIC|Access.STATIC);
  static final Field eofConstant
    = scmSequenceType.addField ("eofValue", typeObject,
				Access.PUBLIC|Access.STATIC);
  static final Method setNameMethod
    = typeProcedure.getDeclaredMethod("setName", 1);
  static Method initIntegerMethod;
  static Method lookupGlobalMethod;
  static Method defineGlobalMethod;
  static Method defineFunctionMethod;
  static Method putGlobalMethod;
  static Method makeListMethod;
  
  public static final Type[] int1Args = { Type.int_type };
  public static final Type[] string1Arg = { javaStringType };
  public static final Type[] sym1Arg = string1Arg;

  static public final Method getSymbolEnvironmentMethod
    = typeEnvironment.addMethod("getSymbol", string1Arg,
				typeSymbol, Access.PUBLIC);

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    initIntegerMethod = javaIntegerType.addMethod ("<init>",
						    int1Args, Type.void_type,
						    Access.PUBLIC);

    lookupGlobalMethod
      = typeEnvironment.addMethod ("lookup_global", sym1Arg,
				   typeObject,
				   Access.PUBLIC|Access.STATIC);
    Type[] symObjArgs = { scmSymbolType, typeObject };
    defineGlobalMethod
      = typeEnvironment.addMethod ("define_global", symObjArgs,
				   Type.void_type,Access.PUBLIC|Access.STATIC);
    defineFunctionMethod
      = typeEnvironment.addMethod ("defineFunction", symObjArgs,
				   Type.void_type,Access.PUBLIC|Access.STATIC);
    putGlobalMethod
      = typeEnvironment.addMethod ("put_global", symObjArgs,
				   Type.void_type,Access.PUBLIC|Access.STATIC);
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
  public static ClassType typeApplet = ClassType.make("java.applet.Applet");
  public static ClassType typeServlet = ClassType.make("gnu.kawa.servlet.KawaServlet");

  public static ClassType typeModuleMethod
  = ClassType.make("gnu.expr.ModuleMethod", typeProcedureN);
  public static ClassType typeApplyMethodProc
  = ClassType.make("gnu.mapping.ApplyMethodProc", typeProcedureN);
  public static ClassType typeApplyMethodContainer
  = ClassType.make("gnu.mapping.ApplyMethodContainer");

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
    = typeValues.addField("noArgs", objArrayType,
				Access.PUBLIC|Access.STATIC);
  public static Field pcCallContextField
    = typeCallContext.addField("pc", Type.int_type, Access.PROTECTED);
  public static ClassType typeCpsProcedure
    = ClassType.make("gnu.mapping.CpsProcedure");
  public static ClassType typeCallFrame
    = ClassType.make("gnu.mapping.CallFrame");
  public static ClassType typeCpsMethodProc
  = ClassType.make("gnu.mapping.CpsMethodProc", typeCpsProcedure);
  public static ClassType typeCpsMethodContainer
  = ClassType.make("gnu.mapping.CpsMethodContainer");
  public static Field numArgsCallFrameField
    = typeCallFrame.addField("numArgs", Type.int_type, Access.PROTECTED);
  public static Field argsCallContextField
    = typeCallContext.addField("values", objArrayType, Access.PROTECTED);
  public static Field procCallContextField
    = typeCallContext.addField("proc", typeProcedure, Access.PROTECTED);
  public static Field callerCallFrameField
    = typeCallFrame.addField("caller", typeCallFrame, Access.PROTECTED);
  public static Field saved_pcCallFrameField
    = typeCallFrame.addField("saved_pc", Type.int_type, Access.PROTECTED);
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

  public static final ClassType getMethodProcType(ClassType modClass)
  {
    return defaultCallConvention >= Compilation.CALL_WITH_CONSUMER
      ? typeCpsMethodProc
      : modClass.getSuperclass().isSubtype(typeProcedure) ? typeModuleMethod
      : typeApplyMethodProc;
  }

  public final ClassType getModuleSuperType(ModuleExp module)
  {
    ClassType sup = module.getSuperType();
    return (sup != null ? sup
	    : usingCPStyle() ? typeCallFrame
	    : generateApplet ? typeApplet
	    : generateServlet ? typeServlet
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
      {
	Literal literal = litTable.findLiteral(value);
	if (literal.field == null)
	  literal.assign(litTable);
	code.emitGetStatic(literal.field);
      }
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

  public boolean inlineOk (Procedure prcc)
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
	getCode().emitGoto(getInterpreter().isTrue(value) ? ctarg.ifTrue
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
		    boolean val = ((PrimType) type).booleanValue(value);
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


  private void emitLiterals()
  {
    if (! immediate && litTable.literalsChain != null)
      {
	try
	  {
	    litTable.emit();
	  }
	catch (Throwable ex)
	  {
	    error('e', "Literals: Internal error:" + ex);
	    ex.printStackTrace(System.err);
	  }
      }
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

  /** If non-null: a prefix for generateClassName to prepend to names. */
  public String classPrefix;

  public static String mangleName (String name)
  {
    return mangleName(name, false);
  }

  public static String mangleNameIfNeeded (String name)
  {
    if (isValidJavaName(name))
      return name;
    else
      return mangleName(name, true);
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

  /** Convert a string to a safe Java identifier.
   * @param reversible if we should use an invertible mapping. */
  public static String mangleName (String name, boolean reversible)
  {
    int len = name.length ();
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
	  mangled.append(reversible ? "$$" : "$");
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

  String source_filename;

  public Compilation (boolean immediate, SourceMessages messages)
  {
    this(messages);
    this.immediate = immediate;
  }

  public Compilation (SourceMessages messages)
  {
    this.messages = messages;
    lexical = new NameLookup(getInterpreter());
  }

  /** Create a new Compilation environment.
   * @param lexp top-level function
   * @param classname name of top-level class to generate
   * @param prefix prefix to pre-pend to the names of other (non-top) classes
   * @param immediate true if the classes will be immediately loaded
   */
  public void compile (ModuleExp lexp, String classname, String prefix)
  {
    source_filename = lexp.filename;
    classPrefix = prefix;
    mainLambda = lexp;

    if (messages.seenErrors())
      return;

    mainClass = new ClassType(classname);

    // Do various code re-writes and optimization.
    PushApply.pushApply(lexp);
    InlineCalls.inlineCalls(lexp, this);
    ChainLambdas.chainLambdas(lexp, this);
    FindTailCalls.findTailCalls(lexp);
    lexp.setCanRead(true);
    FindCapturedVars.findCapturedVars(lexp, this);

    if (messages.seenErrors())
      return;

    if (debugPrintFinalExpr)
      {
	OutPort dout = OutPort.outDefault();
	dout.println ("[Compiling final "+lexp.getName()+" class="+classname+':');
	lexp.print(dout);
	dout.println(']');
	dout.flush();
      }

    mainClass = addClass(lexp, mainClass);
    litTable = new LitTable(this);
    try
      {
	addClass (lexp);
      }
    catch (RuntimeException ex)
      {
	// Try to produce a localized error message.
	error('f', "Internal compiler exception: "+ex);
	throw ex;
      }
  }

  public void compileToFiles (ModuleExp mexp, String topname, String directory, String prefix)
    throws java.io.IOException
  {
    if (directory == null || directory.length() == 0)
      directory = "";
    else if (directory.charAt(directory.length() - 1) != File.separatorChar)
      directory = directory + File.separatorChar;
    String name = mexp.getName();
    if (name != null)
      {
	topname = name;
	if (prefix == null)
	  {
	    int index = name.lastIndexOf('.');
	    if (index >= 0)
	      prefix = name.substring(0, index+1);
	  }
      }

    if (mexp.debugPrintExpr)
      {
	OutPort dout = OutPort.outDefault();
	dout.println("[Compiling module-name:" + mexp.getName()
		      + " top:" + topname + " prefix=" + prefix + " :");
	mexp.print(dout);
	dout.println(']');
	dout.flush();
      }

    /* DEBUGGING:
    OutPort perr = OutPort.errDefault();
    perr.println ("[Expression to compile topname:"+topname+" prefix:"+prefix);
    this.print (perr);
    perr.println();
    perr.flush();
    */

    compile(mexp, topname, prefix);
    outputClass(directory);
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
      }
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
    compile(mexp, LambdaExp.fileFunctionName, null);
    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipOutputStream zout;
    /* Java2:
    if (makeJar)
      zout = new JarOutputStream (new FileOutputStream (zar_file));
    else
    */
      {
	zout = new ZipOutputStream (new FileOutputStream (zar_file));
	zout.setMethod(zout.STORED); // no compression
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

  public void addClass (ClassType new_class)
  {
    if (source_filename != null)
      new_class.setSourceFile (source_filename);
    if (classes == null)
      classes = new ClassType[20];
    else if (numClasses >= classes.length)
      {
	ClassType[] new_classes = new ClassType[2 * classes.length];
	System.arraycopy (classes, 0, new_classes, 0, numClasses);
	classes = new_classes;
      }
    classes[numClasses++] = new_class;
    new_class.access_flags |= Access.PUBLIC|Access.SUPER;
  }

  ClassType allocClass (ModuleExp module)
  {
    String name = module.getJavaName();
    name = generateClassName(name);
    return addClass(module, new ClassType(name));
  }

  ClassType addClass (ModuleExp module, ClassType type)
  {
    ClassType[] interfaces = module.getInterfaces();
    if (interfaces != null)
      type.setInterfaces(interfaces);
    type.setSuper(getModuleSuperType(module));

    module.type = type;
    addClass(type);
    return type;
  }

  public final Method getConstructor (LambdaExp lexp)
  {
    return getConstructor(lexp.getHeapFrameType(), lexp);
  }

  public static final Method getConstructor (ClassType clas, LambdaExp lexp)
  {
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
    ClassType save_class = curClass;
    curClass = clas;
    Method constructor_method = getConstructor(clas, lexp);
    clas.constructor = constructor_method;

    Method superConstructor
      = clas.getSuperclass().addMethod("<init>", Access.PUBLIC,
				       apply0args, Type.void_type);
    method = constructor_method;
    CodeAttr code = constructor_method.startCode();
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);

    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	code.emitPushThis();
	code.emitLoad(code.getCurrentScope().getVariable(1));
	code.emitPutField(lexp.staticLinkField);
      }

    Initializer init;
    lexp.initChain = Initializer.reverse(lexp.initChain);
    if (lexp.initChain != null)
      {
	// Create dummy lambda, for its closureEnv.  This may be needed
	// if init.value contains a reference that uses our heap frame.
	LambdaExp save = curLambda;
	curLambda = new LambdaExp();
	curLambda.closureEnv = code.getArg(0);
	curLambda.outer = save;
	while ((init = lexp.initChain) != null)
	  {
	    lexp.initChain = init.next;
	    init.emit(this);
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
  }

  /** In an <init> for a generated ClassExp, emit $finit$ calls.
   * This recursively traverses superclasses, and also calls their $finit$.
   * @param clas Class to search for $finit$, and to search supertypes.
   * @param seen array of seen classes, to avoid duplicate $finit$ calls.
   */
  private void callInitMethods (ClassType clas, Vector seen)
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

  /** Generate ModuleBody's <tt>apply0</tt>...<tt>applyN</tt> methods.
   * Use the <tt>applyMethods</tt> vector, which contains methods that
   * implement the (public, readable) methods of the current module. */
  public void generateApplyMethods(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    boolean generateApplyMethodContainer
      = ! (curClass.getSuperclass().isSubtype(typeProcedure));
    ClassType procType = getMethodProcType(curClass);
    if (defaultCallConvention >= Compilation.CALL_WITH_CONSUMER)
      curClass.addInterface(typeCpsMethodContainer);
    else if (generateApplyMethodContainer)
      curClass.addInterface(typeApplyMethodContainer);
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
	    else if (defaultCallConvention >= Compilation.CALL_WITH_CONSUMER)
	      {
		methodIndex = numMethods-1;
	      }
	    else // Handling applyN
	      {
		methodIndex = 5 - source.min_args;
		if (methodIndex > 0 && numMethods <= methodIndex && ! varArgs)
		  skipThisProc = true;
		methodIndex = numMethods-1;
	      }
	    if (skipThisProc && ! generateApplyMethodContainer)
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
		else if (defaultCallConvention >= Compilation.CALL_WITH_CONSUMER)
		  {
		    mname = "apply";
		    applyArgs = new Type[2];
		    applyArgs[1] = typeCallContext;
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
		code.emitGetField(procType.getDeclaredField("selector"));
		aswitch = new SwitchState(code);

		needThisApply = true;
	      }
	    if (skipThisProc && generateApplyMethodContainer)
	      continue;

	    aswitch.addCase(source.getSelectorValue(this), code);

	    Method primMethod = primMethods[methodIndex];
	    Type[] primArgTypes = primMethod.getParameterTypes();
	    int nargs = primArgTypes.length;
	    int singleArgs = varArgs ? (nargs - 1) : nargs;
	    Variable counter = null;
	    int pendingIfEnds = 0;
	    boolean withContextArg = primMethod.getName().endsWith("$X");
	    if (withContextArg)
	      singleArgs--;

	    if (i > 4 && numMethods > 1)
	      {
		counter = code.addLocal(Type.int_type);
		if (withContextArg)
		  {
		    code.emitLoad(code.getArg(2)); // get $ctx
		    code.emitGetField(typeCallContext.getDeclaredField("count"));
		  }
		else
		  {
		    code.emitLoad(code.getArg(2));
		    code.emitArrayLength();
		  }
		if (source.min_args != 0)
		  {
		    code.emitPushInt(source.min_args);
		    code.emitSub(Type.int_type);
		  }
		code.emitStore(counter);
	      }

	    int needsThis = primMethod.getStaticFlag() ? 0 : 1;
	    if (needsThis > 0)
	      code.emitPushThis();

	    Declaration var = source.firstDecl();
	    for (int k = 0; k < singleArgs;  k++)
	      {
		if (counter != null && k >= source.min_args)
		  {
		    code.emitLoad(counter);
		    code.emitIfIntLEqZero();
		    if (withContextArg)
		      code.emitLoad(code.getArg(2)); // get $ctx
		    code.emitInvoke(primMethods[k - source.min_args]);
		    code.emitElse();
		    pendingIfEnds++;
		    code.emitInc(counter, (short) (-1));
		  }

		if (withContextArg)
		  {
		    code.emitLoad(code.getArg(2)); // get $ctx
		    code.emitInvoke(typeCallContext.getDeclaredMethod("getNextArg", 0));
		  }
		else if (i > 4) // applyN method
		  {
		    // Load Object[]args value:
		    code.emitLoad(code.getArg(2));
		    code.emitPushInt(k);
		    code.emitArrayLoad(Type.pointer_type);
		  }
		else // apply'i method
		  code.emitLoad(code.getArg(k + 2));
		Type ptype = var.getType();
		if (ptype != Type.pointer_type)
		  CheckedTarget.emitCheckedCoerce(this, source,
						  k, ptype);
		var = var.nextDecl();
	      }

	    if (varArgs)
	      {
		Type lastArgType = primArgTypes[singleArgs];
		if (lastArgType instanceof ArrayType)
		  {
		    Type elType
		      = ((ArrayType) lastArgType).getComponentType();
		    boolean mustConvert
		      = ! "java.lang.Object".equals(elType.getName());
		    if (withContextArg)
		      {
			if (mustConvert)
			  new Error("not implemented mustConvert restarg");
			code.emitLoad(code.getArg(2)); // get $ctx
			code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsArray", 0));
		      }
		    else if (singleArgs == 0 && ! mustConvert)
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
			      (this, source, source.getName(), -1, elType);
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
		    if (withContextArg)
		      {
			code.emitLoad(code.getArg(2)); // get $ctx
			code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsList", 0));
		      }
		    else
		      {
			code.emitLoad(code.getArg(2)); // load args array.
			code.emitPushInt(singleArgs);
			code.emitInvokeStatic(Compilation.makeListMethod);
		      }
		  }
		else if (lastArgType == typeCallContext)
		  code.emitLoad(code.getArg(2));
		else
		  throw new RuntimeException("unsupported #!rest type:"+lastArgType);
              }
	    if (withContextArg)
	      code.emitLoad(code.getArg(2)); // get $ctx
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
		code.emitLoad(code.getArg(1));
		Method errMethod
		  = typeCpsMethodProc.getDeclaredMethod("applyError", 0);
		code.emitInvokeVirtual(errMethod);
	      }
	    else
	      {
		int nargs = i > 4 ? 2 : i + 1;
		nargs++;
		for (int k = generateApplyMethodContainer ? 1 : 0;
		     k < nargs;  k++)
		  code.emitLoad(code.getArg(k));
		if (generateApplyMethodContainer)
		  {
		    mname = mname + "Default";
		    Method defMethod
		      = typeApplyMethodProc.getDeclaredMethod(mname, applyArgs);
		    code.emitInvokeStatic(defMethod);
		  }
		else
		  {
		    code.emitInvokeSpecial(curClass.getSuperclass().getDeclaredMethod(mname, applyArgs));
		  }
	      }
	    code.emitReturn();
	    aswitch.finish(code);
	  }
      }
    method = save_method;
  }

  private Method startClassInit ()
  {
    method = curClass.addMethod ("<clinit>", apply0args, Type.void_type,
				 Access.PUBLIC|Access.STATIC);

    CodeAttr code = method.startCode();

    if (generateMain || generateApplet || generateServlet)
      {
	ClassType interpreterType
	  = (ClassType) Type.make(getInterpreter().getClass());
	Method registerMethod
	  = interpreterType.getDeclaredMethod("registerEnvironment", 0);
	if (registerMethod != null)
	  code.emitInvokeStatic(registerMethod);
      }
    return method;
  }

  /** Compiles a module to a class. */
  public final ClassType addClass (ModuleExp module)
  {
    String name;
    ClassType new_class = module.type;
    if (new_class == typeProcedure)
      new_class = allocClass(module);
    curClass = new_class;

    String filename = module.getFile();
    module.type = new_class;
    if (filename != null)
      new_class.setSourceFile (filename);

    int arg_count;
    char arg_letter;
    LambdaExp saveLambda = curLambda;
    curLambda = module;
    Type[] arg_types;
    if (module.isHandlingTailCalls() || usingCPStyle())
      {
	arg_count = 1;
	arg_letter = '?';
	arg_types = new Type[1];
	arg_types[0] = typeCallContext;
      }
    else if (module.min_args != module.max_args || module.min_args > 4
	|| (fewerClasses && curClass == mainClass))
      {
	arg_count = 1;
	arg_letter = 'N';
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (typeObject);
      }
    else
      {
	arg_count = module.min_args;
	arg_letter = Character.forDigit (arg_count, 10);
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = typeObject;
      }

    CodeAttr code;
    if (arg_letter == 'N' || arg_letter == '?')
      {
	method = curClass.addMethod("numArgs", apply0args, Type.int_type,
				    Access.PUBLIC);
	code = method.startCode();
	code.emitPushInt(module.min_args | (module.max_args << 12));
	code.emitReturn();
      }

    Expression body = module.body;
    Variable heapFrame = module.heapFrame;

    Method apply_method;
    boolean staticModule = false;
    Label classInitLabel = null;
    Label classBodyLabel = null;
    
    if (usingCPStyle())
      {
        apply_method
          = curClass.addMethod ("step", arg_types, Type.void_type, 
                                Access.PUBLIC|Access.FINAL);
      }
    else if (module.isHandlingTailCalls())
      {
	apply_method
	  = curClass.addMethod ("apply", arg_types, Type.void_type,
				Access.PUBLIC|Access.FINAL);
      }
    else
      {
	staticModule = true;
	generateConstructor (module);
	instanceField = curClass.addField("$instance", curClass,
					  Access.STATIC|Access.FINAL);
	apply_method = startClassInit();
	code = getCode();
	code.emitNew(curClass);
	code.emitDup(curClass);
	code.emitInvokeSpecial(curClass.constructor);
	code.emitPutStatic(instanceField);

	classInitLabel = new Label(code);
	classBodyLabel = new Label(code);
	code.emitGoto(classInitLabel);
	classBodyLabel.define(code);
      }
    method = apply_method;

    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();
    // if (usingCPStyle())   code.addParamLocals();

    thisDecl = method.getStaticFlag() ? null : module.declareThis(new_class);
    module.closureEnv = module.thisVariable;
    module.heapFrame = module.thisVariable;
    module.allocChildClasses(this);

    if (module.isHandlingTailCalls() || usingCPStyle())
      {
	Variable callStackContext = new Variable ("$ctx", typeCallContext);
	Scope scope = module.scope;
	scope.addVariableAfter(thisDecl, callStackContext);
	callStackContext.setParameter(true);
	callStackContext.setArtificial(true);
      }

    int line = module.getLine();
    if (line > 0)
      code.putLineNumber(line);

    /*
    if (arg_letter == 'N')
      {
	argsArray.reserveLocal(1, code); // FIXME

	if (true) // If generating code to check number of arguments
	  {
	    code.emitPushThis();
	    code.emitLoad(argsArray);
	    code.emitArrayLength();
	    code.emitInvokeStatic(checkArgCountMethod);
	  }
      }
    */

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

    try
      {
	module.compileBody(this);
      }
    catch (Exception ex)
      {
        error('f', "internal error while compiling - caught: "+ex);
        ex.printStackTrace(System.err);
        System.exit(-1);
      }
    module.compileEnd(this);

    if (Compilation.fewerClasses) // FIXME
      method.popScope(); // Undoes pushScope in method.initCode.

    module.heapFrame = heapFrame;  // Restore heapFrame.
    module.compileChildMethods(this);
    if (usingCPStyle() || (fewerClasses && curClass == mainClass))
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (curClass == mainClass // && ! immediate
	&& (staticModule || clinitChain != null
	    || litTable.literalsChain != null
	    || generateMain || generateApplet || generateServlet))
      {
	Method save_method = method;

	if (staticModule)
	  classInitLabel.define(code);
	else
	  startClassInit();
	code = getCode();
	if (clinitChain != null)
	  {
	    Label lab0 = new Label(code);
	    Label lab1 = new Label(code);
	    Label lab2 = new Label(code);
	    // These gotos are losing.  Should instead do a pre-pass (using
	    // an ExpWalker) before compilation that collects all needed
	    // constants.  Then we generate code to init the literals *first*,
	    // before compiling anything else.  FIXME.
	    code.emitGoto(lab1);
	    lab0.define(code);
	    dumpInitializers(clinitChain);
	    code.emitGoto(lab2);
	    lab1.define(code);
	    emitLiterals();
	    code.emitGoto(lab0);
	    lab2.define(code);
	  }
	else
	  emitLiterals();

	if (staticModule)
	  code.emitGoto(classBodyLabel);
	else
	  code.emitReturn();
	method = save_method;
      }

    curLambda = saveLambda;

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

	code.emitNew(curClass);
	code.emitDup(curClass);
	code.emitInvokeSpecial(curClass.constructor);
	code.emitLoad(code.getArg(0));
	Method moduleMain
	  = typeModuleBody.addMethod("runAsMain", Access.PUBLIC,
				     args, Type.void_type);
	code.emitInvokeVirtual(moduleMain);
	code.emitReturn();
      }

    return new_class;
  }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  /** Generate code to push the current CallContext on the JVM stack. */
  public final void loadCallContext()
  {
    CodeAttr code = getCode();
    if (curLambda.getCallConvention() >= CALL_WITH_CONSUMER)
      {
	Variable var = curLambda.scope.lookup("$ctx");
	if (var != null && var.getType() == typeCallContext)
	  {
	    code.emitLoad(var);
	    return;
	  }
      }
    code.emitInvokeStatic(getCallContextInstanceMethod);
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

  public Interpreter getInterpreter() { return Interpreter.getInterpreter(); }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public final ModuleExp getModule() { return mainLambda; }
  public void setModule(ModuleExp mexp) { mainLambda = mexp; }

  /** The same as getModule, until we allow nested modules. */
  public ModuleExp currentModule() { return current_scope.currentModule(); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled.
   */
  public void mustCompileHere ()
  {
    ScopeExp exp = current_scope;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return;
	if (exp instanceof ModuleExp)
	  {
	    ((ModuleExp) exp).mustCompile = true;
	    return;
	  }
      }
  }

  public ScopeExp currentScope() { return current_scope; }

  public void push (ScopeExp scope)
  {
    if (scope instanceof ModuleExp)
      {
	if (mainLambda == null)
	  mainLambda = (ModuleExp) scope;
      }
    else
      mustCompileHere();
    scope.outer = current_scope;
    current_scope = scope;
    lexical.push(scope);
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
  public void usedClass (ClassType clas)
  {
    if (loader != null && clas.isExisting())
      {
	loader.addClass(clas.getReflectClass());
      }
  }

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message)
  {
    messages.error(severity, getFile(), getLine(), getColumn(),
		   message);
  }

  public void error(char severity, Declaration decl, String msg1, String msg2)
  {
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

  public final String getFile() { return messages.getFile(); }
  public final int getLine() { return messages.getLine(); }
  public final int getColumn() { return messages.getColumn(); }

  public void setFile(String filename) { messages.setFile(filename); }
  public void setLine(int line) { messages.setLine(line); }
  public void setColumn(int column) { messages.setColumn(column); }

  public void setLine(String filename, int line, int column)
  {
    messages.setLine(filename, line, column);
  }

  /** A help vector for building expressions. */
  public Stack exprStack;

  public void letStart ()
  {
    LetExp let = new LetExp(null);
    let.outer = current_scope;
    System.err.println("letStart "+let+" pushed:"+current_scope);
    current_scope = let;
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
    System.err.println("letDone "+let+" outer:"+current_scope);
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
    Expression[] letInits = { loopLambda };
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

  /** Current lexical scope - map name to Declaration. */
  public NameLookup lexical;

  protected ScopeExp current_scope;

  protected SourceMessages messages;
}
