// Copyright (c) 2005  Per M.A. Bothner.
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

  Hashtable table = new Hashtable();

  public Object checkInstance (ModuleInfo info)
  {
    return table.get(info.className);
  }

  /** Allocate a new instance of the class corresponding to the argument. */
  public Object makeInstance (ModuleInfo info)
  {
    String cname = info.className;
    Class clas;
    try
      {
        clas = info.getModuleClass();
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        throw new WrappedException("cannot find module " + cname, ex);
      }

    Object inst;
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
    setInstance(info, inst);
    return inst;
  }

  /** If there is no instance of the argument's class, allocated one. */
  public Object findInstance (ModuleInfo info)
  {
    Object inst = table.get(info.className);
    if (inst == null || info.moduleClass == null
        || ! info.moduleClass.isInstance(inst))
      inst = makeInstance(info);
    return inst;
  }

  public void setInstance (ModuleInfo info, Object instance)
  {
    table.put(info.className, instance);
  }

  public ModuleInfo findFromInstance (Object instance)
  {
    Class instanceClass = instance.getClass();
    String className = instanceClass.getName();
    ModuleInfo info = manager.findWithClassName(className);
    info.moduleClass = instanceClass;
    setInstance(info, instance);
    return info;
  }

}
