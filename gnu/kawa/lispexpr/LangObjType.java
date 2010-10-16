package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.math.*;
import gnu.text.*;
import gnu.kawa.functions.Arithmetic;
import java.util.*;

/** A wrapper around a class type.
 * A LangObjType is implemented using some class type,
 * but may have a custom (language-specific) coercion method,
 * constructor, and name. */

public class LangObjType extends ObjectType implements TypeValue
{
  final int typeCode;
  private static final int PATH_TYPE_CODE = 1;
  private static final int FILEPATH_TYPE_CODE = 2;
  private static final int URI_TYPE_CODE = 3;
  private static final int CLASS_TYPE_CODE = 4;
  private static final int TYPE_TYPE_CODE = 5;
  private static final int CLASSTYPE_TYPE_CODE = 6;
  private static final int INTEGER_TYPE_CODE = 7;
  private static final int RATIONAL_TYPE_CODE = 8;
  private static final int REAL_TYPE_CODE = 9;
  private static final int NUMERIC_TYPE_CODE = 10;
  private static final int LIST_TYPE_CODE = 11;
  private static final int VECTOR_TYPE_CODE = 12;
  private static final int STRING_TYPE_CODE = 13;
  private static final int REGEX_TYPE_CODE = 14;
  private static final int DFLONUM_TYPE_CODE = 15;

  public static final LangObjType pathType =
    new LangObjType("path", "gnu.text.Path",
                    PATH_TYPE_CODE);
  public static final LangObjType filepathType =
    new LangObjType("filepath", "gnu.text.FilePath",
                    FILEPATH_TYPE_CODE);
  public static final LangObjType URIType =
    new LangObjType("URI", "gnu.text.URIPath",
                    URI_TYPE_CODE);

  public static final LangObjType typeClass =
    new LangObjType("class", "java.lang.Class",
                    CLASS_TYPE_CODE);
  public static final LangObjType typeType =
    new LangObjType("type", "gnu.bytecode.Type",
                    TYPE_TYPE_CODE);
  public static final LangObjType typeClassType =
    new LangObjType("class-type", "gnu.bytecode.ClassType",
                    CLASSTYPE_TYPE_CODE);

  public static final LangObjType numericType =
    new LangObjType("number", "gnu.math.Numeric",
                    NUMERIC_TYPE_CODE);

  public static final LangObjType realType =
    new LangObjType("real", "gnu.math.RealNum",
                    REAL_TYPE_CODE);

  public static final LangObjType rationalType =
    new LangObjType("rational", "gnu.math.RatNum",
                    RATIONAL_TYPE_CODE);

  public static final LangObjType integerType =
    new LangObjType("integer", "gnu.math.IntNum",
                    INTEGER_TYPE_CODE);

  public static final LangObjType dflonumType =
    new LangObjType("DFloNum", "gnu.math.DFloNum",
                    DFLONUM_TYPE_CODE);

  public static final LangObjType vectorType =
    new LangObjType("vector", "gnu.lists.FVector",
                    VECTOR_TYPE_CODE);

  public static final LangObjType regexType =
    new LangObjType("regex", "java.util.regex.Pattern",
                    REGEX_TYPE_CODE);

  public static final LangObjType stringType =
    new LangObjType("string",
                    /* #ifdef use:java.lang.CharSequence */
                    "java.lang.CharSequence",
                    /* #else */
                    // /* better would be a union of CharSeq and j.l.String. */
                    // "gnu.lists.CharSeq",
                    /* #endif */
                    STRING_TYPE_CODE);

  public static final LangObjType listType =
    new LangObjType("list", "gnu.lists.LList",
                    LIST_TYPE_CODE);

  static final ClassType typeArithmetic =
    ClassType.make("gnu.kawa.functions.Arithmetic");

  LangObjType(String name, String implClass, int typeCode)
  {
    super(name);
    this.implementationType = ClassType.make(implClass);
    this.typeCode = typeCode;
    this.setSignature(this.implementationType.getSignature());
  }

  ClassType implementationType;

  public int compare(Type other)
  {
    switch (typeCode)
      {
      case CLASS_TYPE_CODE:
        if (other == typeType || other == typeClassType
            || other == typeType.implementationType
            || other == typeClassType.implementationType)
          return -1;
        break;
      case TYPE_TYPE_CODE:
        if (other == typeClass || other == typeClassType
            || other == typeClass.implementationType
            || other == typeClassType.implementationType)
          return 1;
      case CLASSTYPE_TYPE_CODE:
        if (other == typeClass || other == typeClass.implementationType)
          return 1;
        if (other == typeType || other == typeClass.implementationType)
          return -1;
        break;
      case INTEGER_TYPE_CODE:
        if (other instanceof PrimType)
          {
            char sig1 = other.getSignature().charAt(0);
            switch (sig1)
              {
              case 'I': case 'J':  case 'S':  case 'B':
                return 1;
              }
          }
      case DFLONUM_TYPE_CODE:
      case REAL_TYPE_CODE:
        if (other instanceof PrimType)
          {
            char sig1 = other.getSignature().charAt(0);
            switch (sig1)
              {
              case 'D': case 'F':
                return 1;
              }
          }
      }
    return getImplementationType().compare(other.getImplementationType());
  }

  public Field getField(String name, int mask)
  {
    return implementationType.getField(name, mask);
  }

  public Method getMethod(String name, Type[] arg_types)
  {
    return implementationType.getMethod(name, arg_types);
  }

  public Method getDeclaredMethod(String name, int argCount)
  {
    return implementationType.getDeclaredMethod(name, argCount);
  }

  public int getMethods (Filter filter, int searchSupers,
                         /* #ifdef JAVA5 */
                         List<Method>
                         /* #else */
                         // Vector
                         /* #endif */
                         result)
  {
    return implementationType.getMethods(filter, searchSupers, result);
  }

  public java.lang.Class getReflectClass()
  {
    return implementationType.getReflectClass();
  }

  public Type getRealType()
  {
    return implementationType;
  }

  public Type getImplementationType()
  {
    return implementationType;
  }

  static PrimProcedure makePathProc =
    new PrimProcedure("gnu.text.Path", "valueOf", 1);
  static PrimProcedure makeFilepathProc =
    new PrimProcedure("gnu.text.FilePath", "makeFilePath", 1);
  static PrimProcedure makeURIProc =
    new PrimProcedure("gnu.text.URIPath", "makeURI", 1);

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    switch (typeCode)
      {
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case VECTOR_TYPE_CODE:
      case REGEX_TYPE_CODE:
        implementationType.emitIsInstance(comp.getCode());
        target.compileFromStack(comp,
                                comp.getLanguage().getTypeFor(Boolean.TYPE));
        break;
      default:
        gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
      }
  }

  public static Numeric coerceNumeric (Object value)
  {
    Numeric rval = Numeric.asNumericOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, numericType);
    return rval;
  }

  public static RealNum coerceRealNum (Object value)
  {
    RealNum rval = RealNum.asRealNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, realType);
    return rval;
  }

  public static DFloNum coerceDFloNum (Object value)
  {
    DFloNum rval = DFloNum.asDFloNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, dflonumType);
    return rval;
  }

  public static RatNum coerceRatNum (Object value)
  {
    RatNum rval = RatNum.asRatNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, rationalType);
    return rval;
  }

  public static IntNum coerceIntNum (Object value)
  {
    IntNum ival = IntNum.asIntNumOrNull(value);
    if (ival == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, integerType);
    return ival;
  }

  public static Class coerceToClassOrNull (Object type)
  {
    if (type instanceof Class)
      return (Class) type;
    if (type instanceof Type)
      {
        if (type instanceof ClassType
            && ! (type instanceof PairClassType))
          return ((ClassType) type).getReflectClass();
        // FIXME: Handle ArrayType and PrimType.
      }
    return null;
  }

  public static Class coerceToClass (Object obj)
  {
    Class coerced = coerceToClassOrNull(obj);
    if (coerced == null && obj != null)
      throw new ClassCastException("cannot cast "+obj+" to type");
    return coerced;
  }

  public static ClassType coerceToClassTypeOrNull (Object type)
  {
    if (type instanceof ClassType)
      return (ClassType) type;
    if (type instanceof Class)
      {
        Language language = Language.getDefaultLanguage();
        Type t = language.getTypeFor((Class) type);
        if (t instanceof ClassType)
          return (ClassType) t;
      }
    return null;
  }

  public static ClassType coerceToClassType (Object obj)
  {
    ClassType coerced = coerceToClassTypeOrNull(obj);
    if (coerced == null && obj != null)
      throw new ClassCastException("cannot cast "+obj+" to class-type");
    return coerced;
  }

  public static Type coerceToTypeOrNull (Object type)
  {
    if (type instanceof Type)
      return (Type) type;
    if (type instanceof Class)
      {
        Language language = Language.getDefaultLanguage();
        return language.getTypeFor((Class) type);
      }
    return null;
  }

  public static Type coerceToType (Object obj)
  {
    Type coerced = coerceToTypeOrNull(obj);
    if (coerced == null && obj != null)
       throw new ClassCastException("cannot cast "+obj+" to type");
    return coerced;
  }

  Method coercionMethod ()
  {
    switch (typeCode)
      {
      case CLASS_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToClass", 1);
      case CLASSTYPE_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToClassType", 1);
      case TYPE_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToType", 1);
      case NUMERIC_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceNumeric", 1);
      case REAL_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceRealNum", 1);
      case RATIONAL_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceRatNum", 1);
      case INTEGER_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceIntNum", 1);
      case DFLONUM_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceDFloNum", 1);
      case VECTOR_TYPE_CODE:
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case REGEX_TYPE_CODE:
        return null;
      default:
        return ((PrimProcedure) getConstructor()).getMethod();
      }
  }

  Method coercionOrNullMethod()
  {
    ClassType methodDeclaringClass = implementationType;
    String mname;
    switch (typeCode)
      {
      case PATH_TYPE_CODE:
        mname = "coerceToPathOrNull";
        break;
      case FILEPATH_TYPE_CODE:
        mname = "coerceToFilePathOrNull";
        break;
      case URI_TYPE_CODE:
        mname = "coerceToURIPathOrNull";
        break;
      case CLASS_TYPE_CODE:
        methodDeclaringClass = typeLangObjType;
        mname = "coerceToClassOrNull";
        break;
      case CLASSTYPE_TYPE_CODE:
        methodDeclaringClass = typeLangObjType;
        mname = "coerceToClassTypeOrNull";
        break;
      case TYPE_TYPE_CODE:
        methodDeclaringClass = typeLangObjType;
        mname = "coerceToTypeOrNull";
        break;
      case NUMERIC_TYPE_CODE:
        methodDeclaringClass = implementationType;
        mname = "asNumericOrNull";
        break;
      case DFLONUM_TYPE_CODE:
        methodDeclaringClass = implementationType;
        mname = "asDFloNumOrNull";
        break;
      case REAL_TYPE_CODE:
        methodDeclaringClass = implementationType;
        mname = "asRealNumOrNull";
        break;
      case RATIONAL_TYPE_CODE:
        methodDeclaringClass = implementationType;
        mname = "asRatNumOrNull";
        break;
      case INTEGER_TYPE_CODE:
        methodDeclaringClass = implementationType;
        mname = "asIntNumOrNull";
        break;
      default:
        return null;
      }
    return methodDeclaringClass.getDeclaredMethod(mname, 1);
  }

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    Method method = coercionOrNullMethod();
    if (method != null)
      code.emitInvokeStatic(method);
    if (decl != null)
      {
        code.emitDup();
        decl.compileStore(comp);
      }
    if (method != null)
      code.emitIfNotNull();
    else
      {
        implementationType.emitIsInstance(code);
        code.emitIfIntNotZero();
      }
  }

  public Object coerceFromObject (Object obj)
  {
    switch (typeCode)
      {
      case PATH_TYPE_CODE:
        return Path.valueOf(obj);
      case FILEPATH_TYPE_CODE:
        return FilePath.makeFilePath(obj);
      case URI_TYPE_CODE:
        return URIPath.makeURI(obj);
      case CLASS_TYPE_CODE:
        return coerceToClass(obj);
      case CLASSTYPE_TYPE_CODE:
        return coerceToClassType(obj);
      case TYPE_TYPE_CODE:
        return coerceToType(obj);
      case NUMERIC_TYPE_CODE:
        return coerceNumeric(obj);
      case REAL_TYPE_CODE:
        return coerceRealNum(obj);
      case RATIONAL_TYPE_CODE:
        return coerceRatNum(obj);
      case INTEGER_TYPE_CODE:
        return coerceIntNum(obj);
      case DFLONUM_TYPE_CODE:
        return coerceDFloNum(obj);
      case VECTOR_TYPE_CODE:
      case LIST_TYPE_CODE:
      case REGEX_TYPE_CODE:
        // optimize?
      default:
        return super.coerceFromObject(obj);
      }
  }

  public void emitConvertFromPrimitive (Type stackType, CodeAttr code)
  {
    Type argType = null;
    String cname = null;
    switch (typeCode)
      {
      case DFLONUM_TYPE_CODE:
        if (stackType instanceof PrimType)
          {
            if (stackType == Type.intType
                || stackType == Type.byteType
                || stackType == Type.shortType
                || stackType == Type.longType
                || stackType == Type.floatType)
              {
                code.emitConvert(stackType, Type.doubleType);
                stackType = Type.doubleType;
              }
            if (stackType == Type.doubleType)
              {
                 cname = "gnu.math.DFloNum";
                 argType = stackType;
              }
          }
        break;
      case INTEGER_TYPE_CODE:
      case RATIONAL_TYPE_CODE:
      case REAL_TYPE_CODE:
      case NUMERIC_TYPE_CODE:
        if (stackType instanceof PrimType)
          {
            if (stackType == Type.intType
                || stackType == Type.byteType
                || stackType == Type.shortType)
              {
                cname = "gnu.math.IntNum";
                argType = Type.int_type;
              }
            else if (stackType == Type.longType)
              {
                cname = "gnu.math.IntNum";
                argType = Type.long_type;
              }
            else if (typeCode == REAL_TYPE_CODE
                     || typeCode == NUMERIC_TYPE_CODE)
              {
                if (stackType == Type.floatType)
                  {
                    code.emitConvert(Type.float_type, Type.double_type);
                    stackType = Type.doubleType;
                  }
                if (stackType == Type.doubleType)
                  {
                    cname = "gnu.math.DFloNum";
                    argType = Type.doubleType;
                  }
              }
          }
        break;
      }
     if (cname != null)
      {
	ClassType clas = ClassType.make(cname);
	Type[] args = { argType };
	code.emitInvokeStatic(clas.getDeclaredMethod("make", args));
      }
     else
       super.emitConvertFromPrimitive(stackType, code);
  }

  public Expression convertValue (Expression value)
  {
    // In these cases, using the coercion metod would by-pass
    // the static type-checking in InlineCalls#checkType, to no benefit.
    if (typeCode == INTEGER_TYPE_CODE
        || typeCode == NUMERIC_TYPE_CODE
        || typeCode == REAL_TYPE_CODE
        || typeCode == RATIONAL_TYPE_CODE
        || typeCode == DFLONUM_TYPE_CODE)
      return null;
    Method method = coercionMethod();
    if (method == null)
      return null;
    ApplyExp aexp = new ApplyExp(method, new Expression[] { value });
    aexp.setType(this);
    return aexp;
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    switch (typeCode)
      {
      case VECTOR_TYPE_CODE:
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case REGEX_TYPE_CODE:
        code.emitCheckcast(implementationType);
        break;
      default:
        code.emitInvoke(coercionMethod());
      }
  }

  /* #ifdef JAVA5 */
  static final String VARARGS_SUFFIX = "";
  /* #else */
  // static final String VARARGS_SUFFIX = "$V";
  /* #endif */

  public Procedure getConstructor ()
  {
    switch (typeCode)
      {
      case PATH_TYPE_CODE:
        return makePathProc;
      case FILEPATH_TYPE_CODE:
        return makeFilepathProc;
      case URI_TYPE_CODE:
        return makeURIProc;
      case VECTOR_TYPE_CODE:
        return new PrimProcedure("gnu.lists.FVector", "make", 1);
      case LIST_TYPE_CODE:
        return gnu.kawa.functions.MakeList.list;
      case STRING_TYPE_CODE:
        return new PrimProcedure("kawa.lib.strings", "$make$string$"+VARARGS_SUFFIX, 1);
      case REGEX_TYPE_CODE:
        return new PrimProcedure("java.util.regex.Pattern", "compile", 1);
      default:
        return null;
      }
  }

  public static final ClassType typeLangObjType =
    ClassType.make("gnu.kawa.lispexpr.LangObjType");
}
