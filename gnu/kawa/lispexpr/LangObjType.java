package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.*;

public class LangObjType extends ObjectType implements TypeValue
{
  final int typeCode;
  private static final int PATH_TYPE_CODE = 1;
  private static final int FILEPATH_TYPE_CODE = 2;
  private static final int URI_TYPE_CODE = 3;
  private static final int CLASS_TYPE_CODE = 4;
  private static final int TYPE_TYPE_CODE = 5;
  private static final int CLASSTYPE_TYPE_CODE = 6;

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
      }
    return getImplementationType().compare(other.getImplementationType());
  }

  public int getMethods (Filter filter, int searchSupers,
                         java.util.Vector result, String context)
  {
    return implementationType.getMethods(filter, searchSupers, result, context);
  }

  public java.lang.Class getReflectClass()
  {
    return implementationType.getReflectClass();
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
    gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
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

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
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
      default: mname = null;
      }
    code.emitInvokeStatic(methodDeclaringClass.getDeclaredMethod(mname, 1));
    if (decl != null)
      {
        code.emitDup();
        decl.compileStore(comp);
      }
    code.emitIfNotNull();
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
      default: return null;
      }
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    switch (typeCode)
      {
      case CLASS_TYPE_CODE:
        code.emitInvokeStatic(typeLangObjType.getDeclaredMethod("coerceToClass", 1));
        break;
      case CLASSTYPE_TYPE_CODE:
        code.emitInvokeStatic(typeLangObjType.getDeclaredMethod("coerceToClassType", 1));
        break;
      case TYPE_TYPE_CODE:
        code.emitInvokeStatic(typeLangObjType.getDeclaredMethod("coerceToType", 1));
        break;
      default:
        code.emitInvoke(((PrimProcedure) getConstructor()).getMethod());
      }
  }

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
      default: return null;
      }
  }

  public static final ClassType typeLangObjType =
    ClassType.make("gnu.kawa.lispexpr.LangObjType");
}
