package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.math.*;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.Path;
import gnu.kawa.io.URIPath;
import gnu.kawa.functions.Arithmetic;
import gnu.kawa.functions.LProcess;
import gnu.kawa.functions.MakeDynamic;
import gnu.kawa.reflect.CompileBuildObject;
import gnu.kawa.reflect.CompileInvoke;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.LazyType;
import gnu.lists.Blob;
import gnu.lists.FVector;
import gnu.lists.Sequences;
import gnu.lists.U8Vector;
import java.util.*;

/** A wrapper around a class type.
 * A LangObjType is implemented using some class type,
 * but may have a custom (language-specific) coercion method,
 * constructor, and name. */

public class LangObjType extends SpecialObjectType implements TypeValue
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
  private static final int CONST_VECTOR_TYPE_CODE = 13;
  private static final int STRING_TYPE_CODE = 14;
  private static final int REGEX_TYPE_CODE = 15;
  private static final int DFLONUM_TYPE_CODE = 16;
  private static final int S8VECTOR_TYPE_CODE = 17;
  private static final int U8VECTOR_TYPE_CODE = 18;
  private static final int S16VECTOR_TYPE_CODE = 19;
  private static final int U16VECTOR_TYPE_CODE = 20;
  private static final int S32VECTOR_TYPE_CODE = 21;
  private static final int U32VECTOR_TYPE_CODE = 22;
  private static final int S64VECTOR_TYPE_CODE = 23;
  private static final int U64VECTOR_TYPE_CODE = 24;
  private static final int F32VECTOR_TYPE_CODE = 25;
  private static final int F64VECTOR_TYPE_CODE = 26;
  private static final int PROCEDURE_TYPE_CODE = 27;
  private static final int PROMISE_TYPE_CODE = 28;
  private static final int SEQUENCE_TYPE_CODE = 29;
  private static final int DYNAMIC_TYPE_CODE = 30;

  public static final LangObjType pathType =
    new LangObjType("path", "gnu.kawa.io.Path",
                    PATH_TYPE_CODE);
  public static final LangObjType filepathType =
    new LangObjType("filepath", "gnu.kawa.io.FilePath",
                    FILEPATH_TYPE_CODE);
  public static final LangObjType URIType =
    new LangObjType("URI", "gnu.kawa.io.URIPath",
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

  public static final LangObjType constVectorType =
    new LangObjType("constant-vector", "gnu.lists.FVector",
                    CONST_VECTOR_TYPE_CODE);

  public static final LangObjType s8vectorType =
    new LangObjType("s8vector", "gnu.lists.S8Vector",
                    S8VECTOR_TYPE_CODE);

  public static final LangObjType u8vectorType =
    new LangObjType("u8vector", "gnu.lists.U8Vector",
                    U8VECTOR_TYPE_CODE);

  public static final LangObjType s16vectorType =
    new LangObjType("s16vector", "gnu.lists.S16Vector",
                    S16VECTOR_TYPE_CODE);

  public static final LangObjType u16vectorType =
    new LangObjType("u16vector", "gnu.lists.U16Vector",
                    U16VECTOR_TYPE_CODE);

  public static final LangObjType s32vectorType =
    new LangObjType("s32vector", "gnu.lists.S32Vector",
                    S32VECTOR_TYPE_CODE);

  public static final LangObjType u32vectorType =
    new LangObjType("u32vector", "gnu.lists.U32Vector",
                    U32VECTOR_TYPE_CODE);

  public static final LangObjType s64vectorType =
    new LangObjType("s64vector", "gnu.lists.S64Vector",
                    S64VECTOR_TYPE_CODE);

  public static final LangObjType u64vectorType =
    new LangObjType("u64vector", "gnu.lists.U64Vector",
                    U64VECTOR_TYPE_CODE);

  public static final LangObjType f32vectorType =
    new LangObjType("f32vector", "gnu.lists.F32Vector",
                    F32VECTOR_TYPE_CODE);

  public static final LangObjType f64vectorType =
    new LangObjType("f64vector", "gnu.lists.F64Vector",
                    F64VECTOR_TYPE_CODE);

  public static final LangObjType regexType =
    new LangObjType("regex", "java.util.regex.Pattern",
                    REGEX_TYPE_CODE);

  public static final LangObjType stringType =
    new LangObjType("string", "java.lang.CharSequence",
                    STRING_TYPE_CODE);

  public static final LangObjType listType =
    new LangObjType("list", "gnu.lists.LList",
                    LIST_TYPE_CODE);

  static final ClassType typeArithmetic =
    ClassType.make("gnu.kawa.functions.Arithmetic");

  public static final LangObjType procedureType =
    new LangObjType("procedure", "gnu.mapping.Procedure",
                    PROCEDURE_TYPE_CODE);

  public static final LangObjType promiseType =
    new LangObjType("promise", "gnu.mapping.Lazy",
                    PROMISE_TYPE_CODE);

  public static final LangObjType sequenceType =
    new LangObjType("sequence", "java.util.List",
                    SEQUENCE_TYPE_CODE);

    public static final LangObjType dynamicType =
    new LangObjType("dynamic", "java.lang.Object",
                    DYNAMIC_TYPE_CODE);

    LangObjType(String name, String implClass, int typeCode) {
        super(name, ClassType.make(implClass));
        this.typeCode = typeCode;
    }

    public static LangObjType getInstanceFromClass(String name) {
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
        if ("gnu.kawa.io.Path".equals(name))
            return LangObjType.pathType;
        if ("gnu.kawa.io.URIPath".equals(name))
            return LangObjType.URIType;
        if ("gnu.kawa.io.FilePath".equals(name))
            return LangObjType.filepathType;
        if ("java.lang.Class".equals(name))
            return LangObjType.typeClass;
        if ("gnu.bytecode.Type".equals(name))
            return LangObjType.typeType;
        if ("gnu.bytecode.ClassType".equals(name))
            return LangObjType.typeClassType;
        if ("gnu.lists.F64Vector".equals(name))
            return LangObjType.f64vectorType;
        if ("gnu.lists.F32Vector".equals(name))
            return LangObjType.f32vectorType;
        if ("gnu.lists.S64Vector".equals(name))
            return LangObjType.s64vectorType;
        if ("gnu.lists.S32Vector".equals(name))
            return LangObjType.s32vectorType;
        if ("gnu.lists.S16Vector".equals(name))
            return LangObjType.s16vectorType;
        if ("gnu.lists.S8Vector".equals(name))
            return LangObjType.s8vectorType;
        if ("gnu.lists.U64Vector".equals(name))
            return LangObjType.u64vectorType;
        if ("gnu.lists.U32Vector".equals(name))
            return LangObjType.u32vectorType;
        if ("gnu.lists.U16Vector".equals(name))
            return LangObjType.u16vectorType;
        if ("gnu.lists.U8Vector".equals(name))
            return LangObjType.u8vectorType;
        return null;
    }

    @Override
    public int isCompatibleWithValue(Type valueType) {
        if (this == valueType)
            return 2;
        switch (typeCode) {
        case SEQUENCE_TYPE_CODE:
            if (valueType instanceof ArrayType)
                return 1;
            if (stringType.isCompatibleWithValue(valueType) > 0)
                return 1;
            break;
        case DYNAMIC_TYPE_CODE:
            return 2;
        }
        return getImplementationType().isCompatibleWithValue(valueType);
    }

  public int compare(Type other)
  {
    if (other instanceof LazyType)
      other = ((LazyType) other).getValueType();
    if (other == nullType)
      return 1;
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
        if (other == typeType || other == typeClass.implementationType
            || other == procedureType)
          return -1;
        break;
      case PROCEDURE_TYPE_CODE:
        if (other == typeClassType)
          return 1;
        break;
      case SEQUENCE_TYPE_CODE:
          if (other instanceof ArrayType
              || isCompatibleWithValue(stringType, other) > 0)
          return 1;
      }
    return getImplementationType().compare(other);
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    switch (typeCode)
      {
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case VECTOR_TYPE_CODE:
      case S8VECTOR_TYPE_CODE:
      case U8VECTOR_TYPE_CODE:
      case S16VECTOR_TYPE_CODE:
      case U16VECTOR_TYPE_CODE:
      case S32VECTOR_TYPE_CODE:
      case U32VECTOR_TYPE_CODE:
      case S64VECTOR_TYPE_CODE:
      case U64VECTOR_TYPE_CODE:
      case F32VECTOR_TYPE_CODE:
      case F64VECTOR_TYPE_CODE:
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
    value = Promise.force(value);
    Numeric rval = Numeric.asNumericOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, numericType);
    return rval;
  }

  public static RealNum coerceRealNum (Object value)
  {
    value = Promise.force(value);
    RealNum rval = RealNum.asRealNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, realType);
    return rval;
  }

  public static DFloNum coerceDFloNum (Object value)
  {
    value = Promise.force(value);
    DFloNum rval = DFloNum.asDFloNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, dflonumType);
    return rval;
  }

  public static RatNum coerceRatNum (Object value)
  {
    value = Promise.force(value);
    RatNum rval = RatNum.asRatNumOrNull(value);
    if (rval == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, rationalType);
    return rval;
  }

  public static IntNum coerceIntNum (Object value)
  {
    value = Promise.force(value);
    IntNum ival = IntNum.asIntNumOrNull(value);
    if (ival == null && value != null)
        throw new WrongType(WrongType.ARG_CAST, value, integerType);
    return ival;
  }

  public static Class coerceToClassOrNull (Object type)
  {
    type = Promise.force(type);
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
    obj = Promise.force(obj);
    ClassType coerced = coerceToClassTypeOrNull(obj);
    if (coerced == null && obj != null)
      throw new ClassCastException("cannot cast "+obj+" to class-type");
    return coerced;
  }

  public static Type coerceToTypeOrNull (Object type)
  {
    type = Promise.force(type);
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

    public static FVector coerceToConstVector(Object obj) {
        obj = Promise.force(obj);
        if (obj instanceof FVector) {
            FVector vec = (FVector) obj;
            if (vec.isReadOnly())
                return vec;
            throw new ClassCastException("vector is not constant-vector");
        }
       throw new ClassCastException("cannot cast "+obj+" to constant-vector");
    }

  public static Procedure coerceToProcedureOrNull(Object value)
  {
    final Object obj = Promise.force(value);
    if (obj instanceof Procedure)
      return (Procedure) obj;
    if (obj instanceof LangObjType)
      {
        Procedure cons = ((LangObjType) obj).getConstructor();
        if (cons != null)
          return cons;
        return new ProcedureN () {
          public Object applyN (Object[] args) throws Throwable
          {
            int nargs = args.length;
            Object[] xargs = new Object[nargs+1];
            System.arraycopy(args, 0, xargs, 1, nargs);
            xargs[0] = obj;
            return Invoke.make.applyN(xargs);
          }
        };
      }
    return null;
  }

  public static Procedure coerceToProcedure (Object obj)
  {
    obj = Promise.force(obj);
    Procedure coerced = coerceToProcedureOrNull(obj);
    if (coerced == null && obj != null)
       throw new ClassCastException("cannot cast "+obj+" to procedure");
    return coerced;  
  }

    public static U8Vector coerceToU8Vector(Object obj) {
        if (obj instanceof LProcess)
            return ((LProcess) obj).getValue().asPlainBytevector();
        if (obj instanceof Blob)
            return ((Blob) obj).asPlainBytevector();
        return (U8Vector) obj;
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
      case PROCEDURE_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToProcedure", 1);
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
      case U8VECTOR_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToU8Vector", 1);
      case CONST_VECTOR_TYPE_CODE:
        return typeLangObjType.getDeclaredMethod("coerceToConstVector", 1);
      case VECTOR_TYPE_CODE:
      case S8VECTOR_TYPE_CODE:
      case S16VECTOR_TYPE_CODE:
      case U16VECTOR_TYPE_CODE:
      case S32VECTOR_TYPE_CODE:
      case U32VECTOR_TYPE_CODE:
      case S64VECTOR_TYPE_CODE:
      case U64VECTOR_TYPE_CODE:
      case F32VECTOR_TYPE_CODE:
      case F64VECTOR_TYPE_CODE:
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case REGEX_TYPE_CODE:
        return null;
      case SEQUENCE_TYPE_CODE:
        return ClassType.make("gnu.lists.Sequences").getDeclaredMethod("coerceToSequence", 1);
      default:
        Procedure cons = getConstructor();
        if (cons == null)
          return null;
        return ((PrimProcedure) cons).getMethod();
      }
  }

  protected Method coercionOrNullMethod()
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
      case PROCEDURE_TYPE_CODE:
        methodDeclaringClass = typeLangObjType;
        mname = "coerceToProcedureOrNull";
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
      case SEQUENCE_TYPE_CODE:
        methodDeclaringClass = ClassType.make("gnu.lists.Sequences");
        mname = "asSequenceOrNull";
        break;
      default:
        return null;
      }
    return methodDeclaringClass.getDeclaredMethod(mname, 1);
  }

    public boolean emitCoercionOrNull(CodeAttr code) {
        Method method = coercionOrNullMethod();
        if (method == null)
            return false;
        code.emitInvokeStatic(method);
        return true;
    }

    public void emitTestIf(Variable incoming, Declaration decl, Compilation comp) {
        CodeAttr code = comp.getCode();
        if (incoming != null)
            code.emitLoad(incoming);
        if (emitCoercionOrNull(code)) {
            if (decl != null) {
                code.emitDup();
                decl.compileStore(comp);
            }
            code.emitIfNotNull();
        } else {
            implementationType.emitIsInstance(code);
            code.emitIfIntNotZero();
            if (decl != null) {
                code.emitLoad(incoming);
                emitCoerceFromObject(code);
                decl.compileStore(comp);
            }
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
      case PROCEDURE_TYPE_CODE:
        return coerceToProcedure(obj);
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
      case SEQUENCE_TYPE_CODE:
          return Sequences.coerceToSequence(obj);
      case CONST_VECTOR_TYPE_CODE:
          return coerceToConstVector(obj);
      case VECTOR_TYPE_CODE:
      case S8VECTOR_TYPE_CODE:
      case U8VECTOR_TYPE_CODE:
      case S16VECTOR_TYPE_CODE:
      case U16VECTOR_TYPE_CODE:
      case S32VECTOR_TYPE_CODE:
      case U32VECTOR_TYPE_CODE:
      case S64VECTOR_TYPE_CODE:
      case U64VECTOR_TYPE_CODE:
      case F32VECTOR_TYPE_CODE:
      case F64VECTOR_TYPE_CODE:
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
    String mname = "make";
    switch (typeCode)
      {
      case DFLONUM_TYPE_CODE:
        if (stackType instanceof PrimType)
          {
            char sig1 = stackType.getSignature().charAt(0);
            if (sig1 == 'I' || sig1 == 'B' || sig1 == 'S' || sig1 == 'J'
                || sig1 == 'F')
              {
                code.emitConvert((PrimType) stackType, Type.doubleType);
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
                || stackType == Type.shortType
                || stackType == LangPrimType.unsignedByteType
                || stackType == LangPrimType.unsignedShortType)
              {
                cname = "gnu.math.IntNum";
                argType = Type.int_type;
              }
            else if (stackType == Type.longType
                     || stackType == LangPrimType.unsignedIntType
                     || stackType == LangPrimType.unsignedLongType)
              {
                cname = "gnu.math.IntNum";
                mname = stackType == Type.longType ? "valueOf"
                    : "valueOfUnsigned";
                argType = stackType.getImplementationType();
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
	code.emitInvokeStatic(clas.getDeclaredMethod(mname, args));
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
      case S8VECTOR_TYPE_CODE:
      case S16VECTOR_TYPE_CODE:
      case U16VECTOR_TYPE_CODE:
      case S32VECTOR_TYPE_CODE:
      case U32VECTOR_TYPE_CODE:
      case S64VECTOR_TYPE_CODE:
      case U64VECTOR_TYPE_CODE:
      case F32VECTOR_TYPE_CODE:
      case F64VECTOR_TYPE_CODE:
      case STRING_TYPE_CODE:
      case LIST_TYPE_CODE:
      case REGEX_TYPE_CODE:
      case PROMISE_TYPE_CODE:
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

    public ObjectType getConstructorType() {
        switch (typeCode) {
        case PROMISE_TYPE_CODE:
            return LazyType.promiseType;
        default:
            return this;
        }
    }

  public Procedure getConstructor ()
  {
    switch (typeCode)
      {
      case PATH_TYPE_CODE:
        return new PrimProcedure("gnu.kawa.io.Path", "valueOf", 1);
      case FILEPATH_TYPE_CODE:
        return new PrimProcedure("gnu.kawa.io.FilePath", "makeFilePath", 1);
      case URI_TYPE_CODE:
        return new PrimProcedure("gnu.kawa.io.URIPath", "makeURI", 1);
      case VECTOR_TYPE_CODE:
        return new PrimProcedure("gnu.lists.FVector", "make", 1);
      case CONST_VECTOR_TYPE_CODE:
          return new PrimProcedure("gnu.lists.FVector", "makeConstant", 1); 
      case LIST_TYPE_CODE:
        return gnu.kawa.functions.MakeList.list;
      case STRING_TYPE_CODE:
        return new PrimProcedure("kawa.lib.strings", "$make$string$"+VARARGS_SUFFIX, 1);
      case REGEX_TYPE_CODE:
        return new PrimProcedure("java.util.regex.Pattern", "compile", 1);
      case DYNAMIC_TYPE_CODE:
          return MakeDynamic.instance;
      default:
        return null;
      }
  }

    public String encodeType(Language language) {
        if (this == sequenceType) return "sequence";
        return null;
    }

    public Type getElementType() {
        switch (typeCode) {
        case S8VECTOR_TYPE_CODE:  return LangPrimType.byteType; 
        case S16VECTOR_TYPE_CODE: return LangPrimType.shortType; 
        case S32VECTOR_TYPE_CODE: return LangPrimType.intType; 
        case S64VECTOR_TYPE_CODE: return LangPrimType.longType; 
        case U8VECTOR_TYPE_CODE:  return LangPrimType.unsignedByteType; 
        case U16VECTOR_TYPE_CODE: return LangPrimType.unsignedShortType; 
        case U32VECTOR_TYPE_CODE: return LangPrimType.unsignedIntType; 
        case U64VECTOR_TYPE_CODE: return LangPrimType.unsignedLongType; 
        case F32VECTOR_TYPE_CODE: return LangPrimType.floatType; 
        case F64VECTOR_TYPE_CODE: return LangPrimType.doubleType; 
        }
        return null;
    }

    public String elementGetterMethodName() {
        switch (typeCode) {
        case S8VECTOR_TYPE_CODE:
        case U8VECTOR_TYPE_CODE:
            return "getByte";
        case S16VECTOR_TYPE_CODE:
        case U16VECTOR_TYPE_CODE:
            return "getShort";
        case S32VECTOR_TYPE_CODE:
        case U32VECTOR_TYPE_CODE:
            return "getInt";
        case S64VECTOR_TYPE_CODE:
        case U64VECTOR_TYPE_CODE:
            return "getLong";
        case F32VECTOR_TYPE_CODE:
            return "getFloat";
        case F64VECTOR_TYPE_CODE:
            return "getDouble";
        }
        return null;
    }

    public String elementSetterMethodName() {
         String gname = elementGetterMethodName();
         if (gname == null)
             return null;
         return "set" + gname.substring(3);
    }

    public static final ClassType typeLangObjType =
        ClassType.make("gnu.kawa.lispexpr.LangObjType");

    public CompileBuildObject getBuildObject() {
        switch (typeCode) {
        case S8VECTOR_TYPE_CODE:
        case S16VECTOR_TYPE_CODE:
        case S32VECTOR_TYPE_CODE:
        case S64VECTOR_TYPE_CODE:
        case U8VECTOR_TYPE_CODE:
        case U16VECTOR_TYPE_CODE:
        case U32VECTOR_TYPE_CODE:
        case U64VECTOR_TYPE_CODE:
        case F32VECTOR_TYPE_CODE:
        case F64VECTOR_TYPE_CODE:
            return new SimpleVectorBuilder(this);
        default:
            return new CompileBuildObject();
        }
    }

    public static class SimpleVectorBuilder extends CompileBuildObject {
        Type elementType;
        PrimProcedure addProc;
        public SimpleVectorBuilder(LangObjType vectorType) {
            this.elementType = vectorType.getElementType();
            Method addMethod = ((ClassType) vectorType.getImplementationType())
                .getMethod("add",
                           new Type[] { elementType.getImplementationType() });
            addProc = new PrimProcedure(addMethod, Type.voidType,
                                        new Type[] {elementType});
        }

        @Override
        public boolean useBuilder(int numCode, InlineCalls visitor) {
            return (numKeywordArgs() == 0 && hasDefaultConstructor())
                || super.useBuilder(numCode, visitor);
        }

        @Override
        public Expression buildAddChild(Declaration target, Expression child) {
            return new ApplyExp(addProc, new ReferenceExp(target),
                                child);
        }
    }
}
