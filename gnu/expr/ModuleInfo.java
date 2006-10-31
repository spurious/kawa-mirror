// Copyright (c) 2006 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.FieldLocation;
import gnu.text.*;

public class ModuleInfo
{
  /** Next element in list head by {@link ModuleManager#modules}. */
  public ModuleInfo nextModule () { return next; }
  ModuleInfo next;

  /** Name of class that implements module.
   * Must be non-null unless we're currently compiling the module,
   * in which case sourceURL and comp must both be non-null.
   */
  public String className;

  public Class moduleClass;

  /** The namespace URI associated with this module, or {@code null}.
   * This is null for Scheme modules, but non-null for XQuery modules.
   */
  public String getNamespaceUri () { return uri; }
  public void setNamespaceUri (String uri) { this.uri = uri; }
  String uri;

  ModuleExp exp;
  Compilation comp;

  public Compilation getCompilation() { return comp; }

  public void setCompilation (Compilation comp)
  {
    comp.minfo = this;
    this.comp = comp;
    ModuleExp mod = comp.mainLambda;
    this.exp = mod;
    if (mod != null)
      this.sourceURL = mod.getFile();
  }
  
  ModuleInfo[] dependencies;
  int numDependencies;

  /** Location of source for module, if known.
   * This is a absoluete URI, absolute filename,
   * or filename relative to current working directory.
   * Null if source not known; in that case className must be non-null.
   */
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
        if (comp != null)
          return comp.mainLambda;
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
    ClassType type;
    Class rclass;
    if (moduleClass != null)
      {
        rclass = moduleClass;
        type = (ClassType) Type.make(rclass);
      }
    else
      {
        type = ClassType.make(className);
        rclass = type.getReflectClass();
      }
    Object instance = null;

    Language language = Language.getDefaultLanguage();
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

  public static ModuleInfo find (Type type)
  {
    ModuleInfo info = ModuleManager.getInstance().findWithClassName(type.getName());
    if (type instanceof ObjectType && ((ObjectType) type).isExisting())
      {
        try
          {
            info.moduleClass = type.getReflectClass();
          }
        catch (Exception ex)
          {
          }
      }
    return info;
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
            if (getState() == state+1)
              comp.setState(state);
            return false;
          }
      }
    if (getState() == state+1)
      comp.setState(state);
    comp.process(wantedState);
    return getState() == wantedState;
  }

  public void clearClass ()
  {
    moduleClass = null;
    numDependencies = 0;
    dependencies = null;
  }

  /** Check if this module and its dependencies are up-to-dete.
   * Only checks the sourceURL's modification time if it is at least
   * ModifiedCacheTime since last time we checked.
   * As as side-effects update lastModifiedTime and lastCheckedTime.
   */
  public boolean checkCurrent (ModuleManager manager, long now)
  {
    if (lastCheckedTime + manager.lastModifiedCacheTime >= now)
      return true;
    lastCheckedTime = now;
    long lastModifiedTime = URI_utils.lastModified(sourceURL);
    if (moduleClass == null && className != null)
      {
        try
          {
            /* #ifdef JAVA2 */
            moduleClass = Class.forName(className, false,
                                        manager.defaultClassLoader);
            /* #else */
            // moduleClass = Class.forName(className);
            /* #endif */
          }
        catch (ClassNotFoundException ex)
          {
            this.lastModifiedTime = lastModifiedTime;
            return false;
          }
      }
    if (this.lastModifiedTime == 0 && moduleClass != null)
      {
        String classFilename = className;
        int dot = classFilename.lastIndexOf('.');
        if (dot >= 0)
          classFilename = classFilename.substring(dot+1);
        classFilename = classFilename + ".class";
        java.net.URL resource = moduleClass.getResource(classFilename);
        if (resource != null)
          {
            try
              {
                this.lastModifiedTime = resource.openConnection().getLastModified();
              }
            catch (java.io.IOException ex)
              {
                resource = null;
              }
          }
        if (resource == null)
          {
            // Couldn't open timestand of the .class file.
            // Assume it is current.
            this.lastModifiedTime = lastModifiedTime;
            return true;
          }
      }
    if (className == null || lastModifiedTime > this.lastModifiedTime)
      {
        moduleClass = null;
        this.lastModifiedTime = lastModifiedTime;
        return false;
      }
    for (int i = numDependencies;  --i >= 0; )
      {
        ModuleInfo dep = dependencies[i];
        if (! dep.checkCurrent(manager, now))
          return false;
      }
    return true;
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("ModuleInfo[");
    if (moduleClass != null)
      {
        sbuf.append("class: ");
        sbuf.append(moduleClass);
      }
    else if (className != null)
      {
        sbuf.append("class-name: ");
        sbuf.append(className);
      }
    sbuf.append(']');
    return sbuf.toString();
  }
}
