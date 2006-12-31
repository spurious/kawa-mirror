package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.*;

public class LangObjType extends ClassType implements TypeValue
{
  final int typeCode;
  private static final int PATH_TYPE_CODE = 1;
  private static final int FILEPATH_TYPE_CODE = 2;
  private static final int URI_TYPE_CODE = 3;

  public static final LangObjType pathType =
    new LangObjType("path", "gnu.text.Path",
                    PATH_TYPE_CODE);
  public static final LangObjType filepathType =
    new LangObjType("filepath", "gnu.text.FilePath",
                    FILEPATH_TYPE_CODE);
  public static final LangObjType URIType =
    new LangObjType("URI", "gnu.text.URIPath",
                    URI_TYPE_CODE);

  LangObjType(String name, String implClass, int typeCode)
  {
    super(name);
    this.implementationType = ClassType.make(implClass);
    this.typeCode = typeCode;
  }

  ClassType implementationType;

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

 public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
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
      default: mname = null;
      }
    code.emitInvokeStatic(implementationType.getDeclaredMethod(mname, 1));
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
      default: return null;
      }
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitInvoke(((PrimProcedure) getConstructor()).getMethod());
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
}
