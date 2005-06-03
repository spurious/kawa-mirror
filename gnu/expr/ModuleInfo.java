package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location;
import gnu.kawa.reflect.FieldLocation;

public class ModuleInfo
{
  static final String INFO_KEY = new String("(module-info)");

  String className;

  Object instance;

  ModuleExp exp;

  public synchronized ModuleExp getModuleExp ()
  {
    ModuleExp m = exp;
    if (m == null)
      {
        ClassType ctype
          = (instance == null ? ClassType.make(className)
             : (ClassType) ClassType.make(instance.getClass()));
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

  public static ModuleInfo findFromInstance (Object instance)
  {
    ModuleInfo info = find(instance.getClass().getName());
    info.instance = instance;
    return info;
  }

  public static ModuleInfo find (String className)
  {
    // A possibly better interface:  FIXME
    // Use a ModuleManager.  The current MonduleManager is accessed
    // via a parameter object (fluid binding).  The ModuleManager is
    // basically just a hashtable - though it could optionally support
    // inheritance.  Which behaviour is right depends on how we want
    // threads to inherit modules.

    Environment env = Environment.getCurrent();
    Symbol sym = Namespace.EmptyNamespace.getSymbol(className);
    synchronized (env)
      {
	Location loc = env.getLocation(sym, INFO_KEY, true);
	ModuleInfo info = (ModuleInfo) loc.get(null);
	if (info == null)
	  {
	    info = new ModuleInfo();
	    info.className = className;
	    loc.set(info);
	  }
	return info;
      }
  }

  public static void register (Object instance)
  {
    String cname = instance.getClass().getName();
    ModuleInfo info = find(cname);
    info.instance = instance;
  }

  public synchronized Object getInstance ()
  {
    Object inst = instance;
    if (inst == null)
      {
	Class clas;
	String cname = className;
	try
	  {
	    clas = Class.forName(cname);
	  }
	catch (java.lang.ClassNotFoundException ex)
	  {
	    throw new WrappedException("cannot find module " + cname, ex);
	  }

	try
	  {
	    try
	      {
		inst = clas.getDeclaredField("$instance").get(null);
	      }
	    catch (NoSuchFieldException ex)
	      {
		// Not a static module - create a new instance.
		inst = clas.newInstance();
	      }
	  }
	catch (Throwable ex)
	  {
	    throw new WrappedException
	      ("exception while initializing module " + cname, ex);
	  }
	instance = inst;
      }
    return inst;
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
