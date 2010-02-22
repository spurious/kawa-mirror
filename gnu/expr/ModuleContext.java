// Copyright (c) 2005, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.util.*;

/** Maps modules to module instances.
 * Given a class, species a specific instance object for that class.
 */

public class ModuleContext
{
  static ModuleContext global = new ModuleContext(ModuleManager.instance);
  ModuleManager manager;

  public ModuleContext (ModuleManager manager)
  {
    this.manager = manager;
  }

  /** For now returns the shared global ModuleContext.
   * Later provide a means for thread-specific overriding. */
  public static ModuleContext getContext ()
  {
    return global;
  }

  public ModuleManager getManager ()
  {
    return manager;
  }

  /* #ifdef JAVA2 */
  /* #ifdef JAVA5 */
  private static WeakHashMap<Class,Object> table = new WeakHashMap<Class,Object>();
  /* #else */
  // private static WeakHashMap table = new WeakHashMap();
  /* #endif */
  /* #else */
  // private static Hashtable table = new Hashtable();
  /* #endif */

  /** If there is no instance of the argument's class, allocated one. */
  public Object findInstance (ModuleInfo info)
  {
    Class clas;
    try
      {
        clas = info.getModuleClass();
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        String cname = info.getClassName();
        throw new WrappedException("cannot find module " + cname, ex);
      }
    return findInstance(clas);
  }

  public Object searchInstance (Class clas)
  {
    return table.get(clas);
  }

  public Object findInstance (Class clas)
  {
    Object inst = table.get(clas);
    if (inst == null)
      {
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
              ("exception while initializing module " + clas.getName(), ex);
          }
        setInstance(inst);
      }
    return inst;
  }

  public void setInstance (Object instance)
  {
    table.put(instance.getClass(), instance);
  }

  public ModuleInfo findFromInstance (Object instance)
  {
    Class instanceClass = instance.getClass();
    ModuleInfo info = manager.findWithClass(instanceClass);
    setInstance(instance);
    return info;
  }

  /** Remove all entries.
   * This can be used to avoids memory leaks.
   */
  public void clear ()
  {
    table.clear();
  }
}
