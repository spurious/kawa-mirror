// Copyright (c) 1999, 2000, 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.*;
import java.io.*;

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

  public static boolean usingCPStyle;
  public static boolean usingTailCalls = false;

  /** If moduleStatic > 0, (module-static #t) is implied by default.
   * If moduleStatic < 0, (module-static #f) is implied by default. */
  public static int moduleStatic = 0;

  ClassType[] classes;
  int numClasses;

  /** True if the compiled result will be immediately loaded. */ 
  boolean immediate;

  /** The current method. */
  public Method method;

  public final CodeAttr getCode() { return method.getCode(); }

  int method_counter;

  /* When multiple procedures are compiled into a single method,
     we use a switch to jump to the correct part of the method. */
  SwitchState fswitch;

  Field fswitchIndex;
  /** The actual parameter of the current CallFrame.step(CallContext) method. */
  Variable callStackContext;

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
  static public ClassType typeBinding
    = ClassType.make("gnu.mapping.Binding", typeLocation);
  static public final Method getLocationMethod
    = typeLocation.addMethod("get", Type.typeArray0,
			    Type.pointer_type, Access.PUBLIC);
  static public final Method getProcedureBindingMethod
    = typeBinding.addMethod("getProcedure", Type.typeArray0,
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

  static public final Method getBindingEnvironmentMethod
    = typeEnvironment.addMethod("getBinding", string1Arg,
				typeBinding, Access.PUBLIC);

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
  public static Method popCallContextMethod
    = typeCallContext.addMethod("pop", apply0args, Type.void_type,
			      Access.PUBLIC);
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

  Hashtable literalTable;
  int literalsCount;

  /** Rembembers stuff to do in <clinit> of main class. */
  Initializer clinitChain;

  /** Rembembers literals to initialize (in <clinit>). */
  Literal literalsChain;

  public static boolean generateMainDefault = false;
  /** True if we should generate a main(String[]) method. */
  public boolean generateMain = generateMainDefault;

  LitTable litTable;

  public static boolean generateAppletDefault = false;
  /** True if we should generate an Applet. */
  public boolean generateApplet = generateAppletDefault;

  public static final ClassType getMethodProcType(ClassType modClass)
  {
    return usingTailCalls ? typeCpsMethodProc
      : modClass.getSuperclass().isSubtype(typeProcedure) ? typeModuleMethod
      : typeApplyMethodProc;
  }

  public final ClassType getModuleSuperType(ModuleExp module)
  {
    ClassType sup = module.getSuperType();
    return sup != null ? sup : generateApplet ? typeApplet : typeModuleBody;
  }

  public Interpreter getInterpreter()
  {
    return Interpreter.defaultInterpreter;  // For now.  FIXME.
  }

  public Literal findLiteral (Object value)
  {
    if (value == null)
      return Literal.nullLiteral;
    Literal literal = (Literal) literalTable.get (value);
    if (literal == null)
      {
	if (value instanceof Boolean)
	  {
	    boolean val = ((Boolean)value).booleanValue ();
	    literal = new Literal (value,
				   val ? trueConstant : falseConstant,
				   this);
	  }
	else if (value == Values.empty)
	  literal = new Literal (value, voidConstant, this);
	else if (value == gnu.lists.LList.Empty)
	  literal = new Literal (value, emptyConstant, this);
	else if (value == gnu.lists.Sequence.eofValue)
	  literal = new Literal (value, eofConstant, this);
	else if (value instanceof Undefined)
	  literal = new Literal (value, undefinedConstant, this);
	else if (immediate)
	  {
	    literal = new Literal (value, this);
	  }
	else
	  literal = new Literal (value, Type.make(value.getClass()), this);
      }
    return literal;
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
	Literal literal = findLiteral (value);
	if (literal.field == null)
	  literal.assign (this);
	code.emitGetStatic(literal.field);
      }
  }

  public void compileConstant (Object value, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (target instanceof ConsumerTarget)
      {
	if (value == Values.empty)
	  return;
	// else FIXME
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
		      case 'B':  case 'S':  case 'I':
			code.emitPushInt(num.intValue());
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
            error('w', "cannot convert literal (of type "
                  + value.getClass().getName() + ") to "
                  + type.getName());
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

  /** Create a new Compilation environment.
   * @param lexp top-level function
   * @param classname name of top-level class to generate
   * @param prefix prefix to pre-pend to the names of other (non-top) classes
   * @param immediate true if the classes will be immediately loaded
   */
  public Compilation (ModuleExp lexp, String classname, String prefix,
		      boolean immediate)
  {
    source_filename = lexp.filename;
    classPrefix = prefix;
    this.immediate = immediate;
    mainClass = new ClassType(classname);
    mainLambda = lexp;

    // Do various code re-writes and optimization.
    PushApply.pushApply(lexp);
    InlineCalls.inlineCalls(lexp);
    ChainLambdas.chainLambdas(lexp, this);
    FindTailCalls.findTailCalls(lexp);
    lexp.setCanRead(true);
    FindCapturedVars.findCapturedVars(lexp);

    if (debugPrintFinalExpr)
      {
	OutPort dout = OutPort.outDefault();
	dout.println ("[Compiling final "+lexp.getName()+" class="+classname+':');
	lexp.print(dout);
	dout.println(']');
	dout.flush();
      }

    mainClass = addClass(lexp, mainClass);
    literalTable = new Hashtable (100);
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

  ClassType allocClass (LambdaExp lexp)
  {
    String name = lexp.getJavaName();
    name = generateClassName(name);
    if (lexp instanceof ObjectExp)
      return lexp.getCompiledClassType(this);
    else
      return addClass(lexp, new ClassType(name));
  }

  ClassType addClass (LambdaExp lexp, ClassType type)
  {
    ClassType superType;
    if (lexp.isModuleBody ())
      {
        ModuleExp module = (ModuleExp) lexp;
        superType = getModuleSuperType(module);
        ClassType[] interfaces = module.getInterfaces();
        if (interfaces != null)
          type.setInterfaces(interfaces);
      }
    else
      superType = (usingCPStyle ? typeCallFrame
                   : lexp.isHandlingTailCalls() ? typeCpsProcedure
                   : (lexp.min_args != lexp.max_args || lexp.min_args > 4)
                   ? typeProcedureN
                   : typeProcedureArray[lexp.min_args]);
    type.setSuper (superType);

    lexp.type = type;
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
    constructor_method.init_param_slots ();
    CodeAttr code = getCode();
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
    while ((init = lexp.initChain) != null)
      {
	lexp.initChain = init.next;
	init.emit(this);
      }

    if (lexp instanceof ClassExp)
      {
	ClassExp cexp = (ClassExp) lexp;
	LambdaExp initMethod = cexp.initMethod;
	if (initMethod != null)
	  {
	    code.emitPushThis();
	    code.emitInvoke(initMethod.getMainMethod());
	  }
      }

    code.emitReturn();
    method = save_method;
    curClass = save_class;
  }

  /** Generate ModuleBody's apply0 .. applyN methods.
   * Use the applyMethods vector, which contains methods that implement
   * the (public, readable) methods of the current module. */
  public void generateApplyMethods(LambdaExp lexp)
  {
    int numApplyMethods = lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    boolean generateApplyMethodContainer
      = ! (curClass.getSuperclass().isSubtype(typeProcedure));
    ClassType procType
      = Compilation.usingTailCalls ? typeCpsMethodProc
      : generateApplyMethodContainer ? typeApplyMethodProc : typeModuleMethod;
    if (Compilation.usingTailCalls)
      curClass.addInterface(typeCpsMethodContainer);
    else if (generateApplyMethodContainer)
      curClass.addInterface(typeApplyMethodContainer);
    Method save_method = method;
    CodeAttr code = null;
    for (int i = Compilation.usingTailCalls ? 5 : 0;  i < 6; i++)
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
	      || Compilation.usingTailCalls
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
		else if (Compilation.usingTailCalls)
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
					     Compilation.usingTailCalls ? (Type) Type.void_type : (Type) Type.pointer_type,
					     Access.PUBLIC);
		method.init_param_slots();
		code = getCode();

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
	    if (needsThis > 0)
	      code.emitPushThis();

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

		if (i > 4) // applyN method
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
	    if (! usingTailCalls)
	      Target.pushObject.compileFromStack(this,
						 source.getReturnType());
	    code.emitReturn();
          }
	if (needThisApply)
	  {
	    aswitch.addDefault(code);
	    if (usingTailCalls)
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
    method.init_param_slots ();

    CodeAttr code = getCode();

    if (generateMain || generateApplet)
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

  /** Compiles a function to a class. */
  public final ClassType addClass (LambdaExp lexp)
  {
    String name;
    ClassType new_class = lexp.type;
    if (new_class == typeProcedure)
      new_class = allocClass(lexp);
    curClass = new_class;

    String filename = lexp.getFile();
    lexp.type = new_class;
    if (filename != null)
      new_class.setSourceFile (filename);

    int arg_count;
    char arg_letter;
    LambdaExp saveLambda = curLambda;
    curLambda = lexp;
    Type[] arg_types;
    if (lexp.isHandlingTailCalls() || usingCPStyle())
      {
	arg_count = 1;
	arg_letter = '?';
	arg_types = new Type[1];
	arg_types[0] = typeCallContext;
      }
    else if (lexp.min_args != lexp.max_args || lexp.min_args > 4
	|| (fewerClasses && curClass == mainClass))
      {
	arg_count = 1;
	arg_letter = 'N';
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (typeObject);
      }
    else
      {
	arg_count = lexp.min_args;
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
	method.init_param_slots ();
	code = getCode();
	code.emitPushInt(lexp.min_args | (lexp.max_args << 12));
	code.emitReturn();
      }

    Expression body = lexp.body;
    Variable heapFrame = lexp.heapFrame;

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
    else if (lexp.isHandlingTailCalls())
      {
	apply_method
	  = curClass.addMethod ("apply", arg_types, Type.void_type,
				Access.PUBLIC|Access.FINAL);
      }
    else if (lexp.isModuleBody())
      {
	staticModule = true;
	generateConstructor (lexp);
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
    else
      {
	String apply_name = "apply"+arg_letter;
	apply_method
	  = curClass.addMethod (apply_name, arg_types, typeObject,
				Access.PUBLIC|Access.FINAL);
      }
    method = apply_method;

    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();
    // if (usingCPStyle())   code.addParamLocals();

    thisDecl = method.getStaticFlag() ? null : lexp.declareThis(new_class);
    if (lexp instanceof ModuleExp)
      lexp.heapFrame = lexp.thisVariable;
    if (! (fewerClasses && curClass == mainClass))
      lexp.allocChildClasses(this);

    if (lexp.isHandlingTailCalls() || usingCPStyle())
      {
	callStackContext = new Variable ("$ctx", typeCallContext);
	Scope scope = lexp.scope;
	scope.addVariableAfter(thisDecl, callStackContext);
	callStackContext.setParameter(true);
	callStackContext.setArtificial(true);
      }

    int line = lexp.getLine();
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

    lexp.allocParameters(this);
    lexp.enterFunction(this);
    if (usingCPStyle())
      {
	//code.emitLoad(code.getArg(1));
	code.emitLoad(callStackContext);
        code.emitGetField(pcCallContextField);
        fswitch = new SwitchState(code);
      }

    try
      {
	lexp.compileBody(this);
      }
    catch (Exception ex)
      {
        error('f', "internal error while compiling - caught: "+ex);
        ex.printStackTrace(System.err);
        System.exit(-1);
      }
    lexp.compileEnd(this);

    if (Compilation.fewerClasses) // FIXME
      method.popScope(); // Undoes pushScope in method.initCode.

    lexp.heapFrame = heapFrame;  // Restore heapFrame.
    lexp.compileChildMethods(this);
    if (usingCPStyle() || (fewerClasses && curClass == mainClass))
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (Compilation.usingTailCalls && ! staticModule)
      generateConstructor(lexp);

    if (curClass == mainClass // && ! immediate
	&& (staticModule || clinitChain != null || literalsChain != null
	    || generateMain || generateApplet))
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
	    Literal.emit(this);
	    code.emitGoto(lab0);
	    lab2.define(code);
	  }
	else
	  Literal.emit(this);

	if (staticModule)
	  code.emitGoto(classBodyLabel);
	else
	  code.emitReturn();
	method = save_method;
      }

    curLambda = saveLambda;

    if (generateMain && lexp.isModuleBody () && curClass == mainClass)
      {
	Type[] args = { new ArrayType(javaStringType) };
	method = curClass.addMethod("main", Access.PUBLIC|Access.STATIC,
				    args, Type.void_type);
				    
	method.init_param_slots ();
	code = getCode();
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

  public static boolean usingCPStyle() { return usingCPStyle; }
  public static boolean usingTailCalls() { return usingTailCalls; }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  public void freeLocalField (Field field)
  {
    // FIXME
  }

  String filename;
  int position;

  public void error(char severity, String message)
  {
    error(severity, filename, position >> 12, position & ((1 << 12) - 1), message);
  }

  public void error(char severity, String filename, int line, int column, String message)
  {
    error(new gnu.text.SourceError(severity, filename, line, column, message));
  }

  public void error(gnu.text.SourceError err)
  {
    // FIXME - use SourceMessages framework!
    System.err.println(err);
  }
}
