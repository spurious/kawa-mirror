package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.FieldLocation;
import gnu.text.*;

public class ModuleInfo
{
  /** Next element in list head by ModuleManager.modules. */
  ModuleInfo next;

  public String className;

  ModuleExp exp;
  Compilation comp;

  public Compilation getCompilation() { return comp; }
  
  public Class moduleClass;

  ModuleInfo[] dependencies;
  int numDependencies;

  /** Maybe just use a a URL?. */
  public String sourceURL;
  public long lastCheckedTime;
  public long lastModifiedTime;

  public synchronized void addDependency (ModuleInfo dep)
  {
    if (dependencies == null)
      dependencies = new ModuleInfo[8];
    else if (numDependencies == dependencies.length)
      {
        ModuleInfo[] deps = new ModuleInfo[2 * numDependencies];
        System.arraycopy(dependencies, 0, deps, 0, numDependencies);
        dependencies = deps;
      }
    dependencies[numDependencies++] = dep;
  }

  public ClassType getClassType ()
  {
    if (moduleClass != null)
      return (ClassType) Type.make(moduleClass);
    if (comp != null && comp.mainClass != null)
      return comp.mainClass;
    return ClassType.make(className);
  }

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
        fref.setDontDereference(true);
        fref.setFlag(ReferenceExp.CREATE_FIELD_REFERENCE);
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

  public int getState () { return comp == null ? Compilation.CLASS_WRITTEN : comp.getState(); }

  public void loadByStages (int wantedState)
  {
    int state = getState();
    if (state + 1 >= wantedState)
      return;
    loadByStages(wantedState - 2);
    state = getState();
    if (state >= wantedState) // Most likely? if ERROR_SEEN.
      return;
    comp.setState(state+1);
    int ndeps = numDependencies;
    for (int idep = 0;  idep < ndeps;  idep++)
      {
        ModuleInfo dep = dependencies[idep];
        dep.loadByStages(wantedState);
      }
    state = getState();
    if (state >= wantedState) // Most likely? if ERROR_SEEN.
      return;
    comp.setState(state & ~1);
    comp.process(wantedState);
  }

  /** Eagerly process the module and dependencies.
   * @return true on success; false if we were unable to because of
   * an error or a cyclic dependency.
   */
  public boolean loadEager (int wantedState)
  {
    if (comp == null && className != null)
      return false;
    int state = getState();
    if (state >= wantedState)
      return true;
    if ((state & 1) != 0)
      return false;
    comp.setState(state + 1);
    int ndeps = numDependencies;
    for (int idep = 0;  idep < ndeps;  idep++)
      {
        ModuleInfo dep = dependencies[idep];
        if (! dep.loadEager(wantedState))
          {
            comp.setState(state);
            return false;
          }
      }
    if (getState() == state+1)
      comp.setState(state);
    comp.process(wantedState);
    return getState() == wantedState;
  }
}
