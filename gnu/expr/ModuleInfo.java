package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.FieldLocation;

public class ModuleInfo
{
  /** Next element in list head by ModuleManager.modules. */
  ModuleInfo next;

  public String className;

  ModuleExp exp;

  public Class moduleClass;

  /** Maybe just use a a URL?. */
  public String sourceURL;
  public long lastCheckedTime;
  public long lastModifiedTime;

  public synchronized ModuleExp getModuleExp ()
  {
    ModuleExp m = exp;
    if (m == null)
      {
        ClassType ctype = ClassType.make(className);
        m = new ModuleExp();
        m.type = ctype;
        m.setName(ctype.getName());
        m.flags |= ModuleExp.LAZY_DECLARATIONS;
        m.info = this;
        exp = m;
      }
    return m;
  }

  /** If module has LAZY_DECLARATIONS, fix that. */
  public synchronized ModuleExp setupModuleExp ()
  {
    ModuleExp mod = getModuleExp();
    if ((mod.flags & ModuleExp.LAZY_DECLARATIONS) == 0)
      return mod;
    mod.setFlag(false, ModuleExp.LAZY_DECLARATIONS);
    ClassType type = ClassType.make(className);
    Object instance = null;

    Language language = Language.getDefaultLanguage();
    Class rclass = type.getReflectClass();
    for (Field fld = type.getFields();  fld != null;  fld = fld.getNext())
      {
	int flags = fld.getFlags();
	if ((flags & Access.PUBLIC) == 0)
	  continue;
	try
	  {
            if ((flags & Access.STATIC) == 0 && instance == null)
              instance = getInstance();
            Object fvalue = rclass.getField(fld.getName()).get(instance);

            Declaration fdecl = language.declFromField(mod, fvalue, fld);
            if ((flags & Access.FINAL) != 0
                && (! (fvalue instanceof gnu.mapping.Location)
                    || fvalue instanceof FieldLocation))
              fdecl.noteValue(new QuoteExp(fvalue));
            else
              fdecl.noteValue(null);
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }

    for (Declaration fdecl = mod.firstDecl();
         fdecl != null;  fdecl = fdecl.nextDecl())
      {
        makeDeclInModule2(mod, fdecl);
      }
    return mod;
  }

  public Class getModuleClass ()
    throws ClassNotFoundException
  {
    Class mclass = moduleClass;
    if (mclass != null)
      return mclass;
    mclass = Class.forName(className);
    moduleClass = mclass;
    return mclass;
  }

  public static ModuleInfo findFromInstance (Object instance)
  {
    return ModuleContext.getContext().findFromInstance(instance);
  }

  public static ModuleInfo find (String className)
  {
    return ModuleManager.getInstance().findWithClassName(className);
  }

  public static void register (Object instance)
  {
    ModuleInfo info = find(instance.getClass().getName());
    ModuleContext.getContext().setInstance(info, instance);
  }

  public Object getInstance ()
  {
    return ModuleContext.getContext().findInstance(this);
  }

  public Object getRunInstance ()
  {
    Object inst = getInstance();
    if (inst instanceof Runnable)
      ((Runnable) inst).run();
    return inst;
  }

  static void makeDeclInModule2 (ModuleExp mod, Declaration fdecl)
  {
    Object fvalue = fdecl.getConstantValue();
    if (fvalue instanceof FieldLocation)
      {
	FieldLocation floc = (FieldLocation) fvalue;
        Declaration vdecl = floc.getDeclaration();
        ReferenceExp fref = new ReferenceExp(vdecl);
        fdecl.setAlias(true);
        if (floc.isIndirectLocation())
          fref.setDontDereference(true);
        fdecl.setValue(fref);
        if (vdecl.isProcedureDecl())
          fdecl.setProcedureDecl(true);
        if (vdecl.getFlag(Declaration.IS_SYNTAX))
          fdecl.setSyntax();
        if (! fdecl.getFlag(Declaration.STATIC_SPECIFIED))
          {
            ClassType vtype = floc.getDeclaringClass();
            String vname = vtype.getName();
            for (Declaration xdecl = mod.firstDecl();
                 xdecl != null;  xdecl = xdecl.nextDecl())
              {
                if (vname.equals(xdecl.getType().getName())
                    && xdecl.getFlag(Declaration.MODULE_REFERENCE))
                  {
                    fref.setContextDecl(xdecl);
                    break;
                  }
              }
          }
      }
  }
}
