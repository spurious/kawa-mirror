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

  Hashtable table = new Hashtable();

  public Object checkInstance (ModuleInfo info)
  {
    return table.get(info.className);
  }

  public Object findInstance (ModuleInfo info)
  {
    Object inst = table.get(info.className);
    if (inst == null)
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
      }
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
